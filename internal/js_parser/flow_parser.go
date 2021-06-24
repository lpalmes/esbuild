//const sum = (a, b) => a + b;\n This file contains code for parsing Flow syntax. The parser just skips
// over type expressions as if they are whitespace and doesn't bother generating
// an AST because nothing uses type information.

package js_parser

import (
	"github.com/lpalmes/esbuild/internal/js_ast"
	"github.com/lpalmes/esbuild/internal/js_lexer"
)

func (p *parser) skipFlowBinding() {
	switch p.lexer.Token {
	case js_lexer.TIdentifier, js_lexer.TThis:
		p.lexer.Next()

	case js_lexer.TOpenBracket:
		p.lexer.Next()

		// "[, , a]"
		for p.lexer.Token == js_lexer.TComma {
			p.lexer.Next()
		}

		// "[a, b]"
		for p.lexer.Token != js_lexer.TCloseBracket {
			p.skipFlowBinding()
			if p.lexer.Token != js_lexer.TComma {
				break
			}
			p.lexer.Next()
		}

		p.lexer.Expect(js_lexer.TCloseBracket)

	case js_lexer.TOpenBrace:
		p.lexer.Next()

		for p.lexer.Token != js_lexer.TCloseBrace {
			foundIdentifier := false

			switch p.lexer.Token {
			case js_lexer.TIdentifier:
				// "{x}"
				// "{x: y}"
				foundIdentifier = true
				p.lexer.Next()

				// "{1: y}"
				// "{'x': y}"
			case js_lexer.TStringLiteral, js_lexer.TNumericLiteral:
				p.lexer.Next()

			default:
				if p.lexer.IsIdentifierOrKeyword() {
					// "{if: x}"
					p.lexer.Next()
				} else {
					p.lexer.Unexpected()
				}
			}

			if p.lexer.Token == js_lexer.TColon || !foundIdentifier {
				p.lexer.Expect(js_lexer.TColon)
				p.skipFlowBinding()
			}

			if p.lexer.Token != js_lexer.TComma {
				break
			}
			p.lexer.Next()
		}

		p.lexer.Expect(js_lexer.TCloseBrace)

	default:
		p.lexer.Unexpected()
	}
}

func (p *parser) skipFlowFnArgs() {
	p.lexer.Expect(js_lexer.TOpenParen)

	for p.lexer.Token != js_lexer.TCloseParen {
		// "(...a)"
		if p.lexer.Token == js_lexer.TDotDotDot {
			p.lexer.Next()
		}

		p.skipFlowBinding()

		// "(a?)"
		if p.lexer.Token == js_lexer.TQuestion {
			p.lexer.Next()
		}

		// "(a: any)"
		if p.lexer.Token == js_lexer.TColon {
			p.lexer.Next()
			p.skipFlowType(js_ast.LLowest)
		}

		// "(a, b)"
		if p.lexer.Token != js_lexer.TComma {
			break
		}
		p.lexer.Next()
	}

	p.lexer.Expect(js_lexer.TCloseParen)
}

// This is a spot where the Flow grammar is highly ambiguous. Here are
// some cases that are valid:
//
//     let x = (y: any): (() => {}) => { };
//     let x = (y: any): () => {} => { };
//     let x = (y: any): (y) => {} => { };
//     let x = (y: any): (y[]) => {};
//     let x = (y: any): (a | b) => {};
//
// Here are some cases that aren't valid:
//
//     let x = (y: any): (y) => {};
//     let x = (y: any): (y) => {return 0};
//     let x = (y: any): asserts y is (y) => {};
//
func (p *parser) skipFlowParenOrFnType() {
	if p.trySkipFlowArrowArgsWithBacktracking() {
		p.skipFlowReturnType()
	} else {
		p.lexer.Expect(js_lexer.TOpenParen)
		p.skipFlowType(js_ast.LLowest)
		p.lexer.Expect(js_lexer.TCloseParen)
	}
}

func (p *parser) skipFlowReturnType() {
	p.skipFlowTypeWithOpts(js_ast.LLowest, skipTypeOpts{isReturnType: true})
}

func (p *parser) skipFlowType(level js_ast.L) {
	p.skipFlowTypeWithOpts(level, skipTypeOpts{})
}

func (p *parser) skipFlowTypeWithOpts(level js_ast.L, opts skipTypeOpts) {
	for {
		switch p.lexer.Token {
		// Optional type:  type A = ?string;
		case js_lexer.TQuestion:
			p.lexer.Next()
			continue

			// Infer type notation: function foo(): Array<*>
		case js_lexer.TAsterisk:
			p.lexer.Next()
			return

		case js_lexer.TNumericLiteral, js_lexer.TBigIntegerLiteral, js_lexer.TStringLiteral,
			js_lexer.TNoSubstitutionTemplateLiteral, js_lexer.TTrue, js_lexer.TFalse,
			js_lexer.TNull, js_lexer.TVoid, js_lexer.TConst:
			p.lexer.Next()

		case js_lexer.TThis:
			p.lexer.Next()

			// "function check(): this is boolean"
			if p.lexer.IsContextualKeyword("is") && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
				return
			}

		case js_lexer.TMinus:
			// "-123"
			// "-123n"
			p.lexer.Next()
			if p.lexer.Token == js_lexer.TBigIntegerLiteral {
				p.lexer.Next()
			} else {
				p.lexer.Expect(js_lexer.TNumericLiteral)
			}

		case js_lexer.TAmpersand:
		case js_lexer.TBar:
			// Support things like "type Foo = | A | B" and "type Foo = & A & B"
			p.lexer.Next()
			continue

		case js_lexer.TImport:
			// "import('fs')"
			p.lexer.Next()
			p.lexer.Expect(js_lexer.TOpenParen)
			p.lexer.Expect(js_lexer.TStringLiteral)
			p.lexer.Expect(js_lexer.TCloseParen)

		case js_lexer.TNew:
			// "new () => Foo"
			// "new <T>() => Foo<T>"
			p.lexer.Next()
			p.skipFlowTypeParameters()
			p.skipFlowParenOrFnType()

		case js_lexer.TLessThan:
			// "<T>() => Foo<T>"
			p.skipFlowTypeParameters()
			p.skipFlowParenOrFnType()

		case js_lexer.TOpenParen:
			// "(number | string)"
			p.skipFlowParenOrFnType()

		case js_lexer.TIdentifier:
			kind := tsTypeIdentifierMap[p.lexer.Identifier]

			if kind == tsTypeIdentifierPrefix {
				p.lexer.Next()
				p.skipFlowType(js_ast.LPrefix)
				break
			}

			checkTypeParameters := true

			if kind == tsTypeIdentifierUnique {
				p.lexer.Next()

				// "let foo: unique symbol"
				if p.lexer.IsContextualKeyword("symbol") {
					p.lexer.Next()
					break
				}
			} else if kind == tsTypeIdentifierAbstract {
				p.lexer.Next()

				// "let foo: abstract new () => {}" added in Flow 4.2
				if p.lexer.Token == js_lexer.TNew {
					continue
				}
			} else if kind == tsTypeIdentifierAsserts {
				p.lexer.Next()

				// "function assert(x: boolean): asserts x"
				// "function assert(x: boolean): asserts x is boolean"
				if opts.isReturnType && !p.lexer.HasNewlineBefore && (p.lexer.Token == js_lexer.TIdentifier || p.lexer.Token == js_lexer.TThis) {
					p.lexer.Next()
				}
			} else if kind == tsTypeIdentifierPrimitive {
				p.lexer.Next()
				checkTypeParameters = false
			} else {
				p.lexer.Next()
			}

			// "function assert(x: any): x is boolean"
			if p.lexer.IsContextualKeyword("is") && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
				return
			}

			// "let foo: any \n <number>foo" must not become a single type
			if checkTypeParameters && !p.lexer.HasNewlineBefore {
				p.skipFlowTypeArguments(false /* isInsideJSXElement */)
			}

		case js_lexer.TTypeof:
			p.lexer.Next()
			if p.lexer.Token == js_lexer.TImport {
				// "typeof import('fs')"
				continue
			} else {
				// "typeof x"
				// "typeof x.y"
				for {
					if !p.lexer.IsIdentifierOrKeyword() {
						p.lexer.Expected(js_lexer.TIdentifier)
					}
					p.lexer.Next()
					if p.lexer.Token != js_lexer.TDot {
						break
					}
					p.lexer.Next()
				}
			}

		case js_lexer.TOpenBracket:
			// "[number, string]"
			// "[first: number, second: string]"
			p.lexer.Next()
			for p.lexer.Token != js_lexer.TCloseBracket {
				if p.lexer.Token == js_lexer.TDotDotDot {
					p.lexer.Next()
				}
				p.skipFlowType(js_ast.LLowest)
				if p.lexer.Token == js_lexer.TQuestion {
					p.lexer.Next()
				}
				if p.lexer.Token == js_lexer.TColon {
					p.lexer.Next()
					p.skipFlowType(js_ast.LLowest)
				}
				if p.lexer.Token != js_lexer.TComma {
					break
				}
				p.lexer.Next()
			}
			p.lexer.Expect(js_lexer.TCloseBracket)

		case js_lexer.TOpenBrace:
			p.skipFlowObjectType()

		case js_lexer.TTemplateHead:
			// "`${'a' | 'b'}-${'c' | 'd'}`"
			for {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
				p.lexer.RescanCloseBraceAsTemplateToken()
				if p.lexer.Token == js_lexer.TTemplateTail {
					p.lexer.Next()
					break
				}
			}

		default:
			p.lexer.Unexpected()
		}
		break
	}

	for {
		switch p.lexer.Token {
		case js_lexer.TBar:
			if level >= js_ast.LBitwiseOr {
				return
			}
			p.lexer.Next()
			// Check if we are in an object closing brace |}
			if p.lexer.Token == js_lexer.TCloseBrace {
				return
			}
			p.skipFlowType(js_ast.LBitwiseOr)

		case js_lexer.TAmpersand:
			if level >= js_ast.LBitwiseAnd {
				return
			}
			p.lexer.Next()
			p.skipFlowType(js_ast.LBitwiseAnd)

		case js_lexer.TExclamation:
			// A postfix "!" is allowed in JSDoc types in Flow, which are only
			// present in comments. While it's not valid in a non-comment position,
			// it's still parsed and turned into a soft error by the Flow
			// compiler. It turns out parsing this is important for correctness for
			// "as" casts because the "!" token must still be consumed.
			if p.lexer.HasNewlineBefore {
				return
			}
			p.lexer.Next()

		case js_lexer.TDot:
			p.lexer.Next()
			if !p.lexer.IsIdentifierOrKeyword() {
				p.lexer.Expect(js_lexer.TIdentifier)
			}
			p.lexer.Next()
			p.skipFlowTypeArguments(false /* isInsideJSXElement */)

		case js_lexer.TOpenBracket:
			// "{ ['x']: string \n ['y']: string }" must not become a single type
			if p.lexer.HasNewlineBefore {
				return
			}
			p.lexer.Next()
			if p.lexer.Token != js_lexer.TCloseBracket {
				p.skipFlowType(js_ast.LLowest)
			}
			p.lexer.Expect(js_lexer.TCloseBracket)

		case js_lexer.TExtends:
			// "{ x: number \n extends: boolean }" must not become a single type
			if p.lexer.HasNewlineBefore || level >= js_ast.LConditional {
				return
			}
			p.lexer.Next()

			// The type following "extends" is not permitted to be another conditional type
			p.skipFlowType(js_ast.LConditional)
			p.lexer.Expect(js_lexer.TQuestion)
			p.skipFlowType(js_ast.LLowest)
			p.lexer.Expect(js_lexer.TColon)
			p.skipFlowType(js_ast.LLowest)

		default:
			return
		}
	}
}

func (p *parser) skipFlowObjectType() {
	p.lexer.Expect(js_lexer.TOpenBrace)

	// If we have a double bar this means an empty exact object type Obj = {||}
	// so we early return here
	if p.lexer.Token == js_lexer.TBarBar {
		p.lexer.Next()
		p.lexer.Expect(js_lexer.TCloseBrace)
		return
	}

	// Track if we are in a flow exact object
	exactObject := false

	// type Object = {| ........ |}
	if p.lexer.Token == js_lexer.TBar {
		exactObject = true
		p.lexer.Next()
	}

	for p.lexer.Token != js_lexer.TCloseBrace {

		if exactObject && p.lexer.Token == js_lexer.TBar {
			// This object is exact and ends in | so we just break from the loop and expect
			// a } at the end
			p.lexer.Next()
			break
		}

		// Spread type into another object {| ...AnotherObject |}
		if p.lexer.Token == js_lexer.TDotDotDot {
			p.lexer.Next()
			if p.lexer.Token == js_lexer.TIdentifier {
				p.lexer.Next()
			}

			switch p.lexer.Token {
			case js_lexer.TCloseBrace:

			case js_lexer.TComma, js_lexer.TSemicolon:
				p.lexer.Next()

			}
			continue
		}

		// "{ -readonly [K in keyof T]: T[K] }"
		// "{ +readonly [K in keyof T]: T[K] }"
		if p.lexer.Token == js_lexer.TPlus || p.lexer.Token == js_lexer.TMinus {
			p.lexer.Next()
		}

		// Skip over modifiers and the property identifier
		foundKey := false
		for p.lexer.IsIdentifierOrKeyword() ||
			p.lexer.Token == js_lexer.TStringLiteral ||
			p.lexer.Token == js_lexer.TNumericLiteral {
			p.lexer.Next()
			foundKey = true
		}

		if p.lexer.Token == js_lexer.TOpenBracket {
			// Index signature or computed property
			p.lexer.Next()
			p.skipFlowType(js_ast.LLowest)

			// "{ [key: string]: number }"
			// "{ readonly [K in keyof T]: T[K] }"
			if p.lexer.Token == js_lexer.TColon {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
			} else if p.lexer.Token == js_lexer.TIn {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
				if p.lexer.IsContextualKeyword("as") {
					// "{ [K in keyof T as `get-${K}`]: T[K] }"
					p.lexer.Next()
					p.skipFlowType(js_ast.LLowest)
				}
			}

			p.lexer.Expect(js_lexer.TCloseBracket)

			// "{ [K in keyof T]+?: T[K] }"
			// "{ [K in keyof T]-?: T[K] }"
			if p.lexer.Token == js_lexer.TPlus || p.lexer.Token == js_lexer.TMinus {
				p.lexer.Next()
			}

			foundKey = true
		}

		// "?" indicates an optional property
		// "!" indicates an initialization assertion
		if foundKey && (p.lexer.Token == js_lexer.TQuestion || p.lexer.Token == js_lexer.TExclamation) {
			p.lexer.Next()
		}

		// Type parameters come right after the optional mark
		p.skipFlowTypeParameters()

		switch p.lexer.Token {
		case js_lexer.TColon:
			// Regular property
			if !foundKey {
				p.lexer.Expect(js_lexer.TIdentifier)
			}
			p.lexer.Next()
			p.skipFlowType(js_ast.LLowest)

		case js_lexer.TOpenParen:
			// Method signature
			p.skipFlowFnArgs()
			if p.lexer.Token == js_lexer.TColon {
				p.lexer.Next()
				p.skipFlowReturnType()
			}

		default:
			if !foundKey {
				p.lexer.Unexpected()
			}
		}

		switch p.lexer.Token {
		case js_lexer.TCloseBrace:

		case js_lexer.TComma, js_lexer.TSemicolon:
			p.lexer.Next()

		default:
			if !p.lexer.HasNewlineBefore {
				p.lexer.Unexpected()
			}
		}
	}

	// We ignore checking the exact object since skipFlowType above consumes the bar
	// From |} so we just need to expect a closing }
	p.lexer.Expect(js_lexer.TCloseBrace)
}

// This is the type parameter declarations that go with other symbol
// declarations (class, function, type, etc.)
func (p *parser) skipFlowTypeParameters() {
	if p.lexer.Token == js_lexer.TLessThan {
		p.lexer.Next()

		for {
			p.lexer.Expect(js_lexer.TIdentifier)

			// "class Foo<T: AnotherType> {}"
			if p.lexer.Token == js_lexer.TColon {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
			}

			// "class Foo<T extends number> {}"
			if p.lexer.Token == js_lexer.TExtends {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
			}

			// "class Foo<T = void> {}"
			if p.lexer.Token == js_lexer.TEquals {
				p.lexer.Next()
				p.skipFlowType(js_ast.LLowest)
			}

			if p.lexer.Token != js_lexer.TComma {
				break
			}
			p.lexer.Next()
			if p.lexer.Token == js_lexer.TGreaterThan {
				break
			}
		}

		p.lexer.ExpectGreaterThan(false /* isInsideJSXElement */)
	}
}

func (p *parser) skipFlowTypeArguments(isInsideJSXElement bool) bool {
	switch p.lexer.Token {
	case js_lexer.TLessThan, js_lexer.TLessThanEquals,
		js_lexer.TLessThanLessThan, js_lexer.TLessThanLessThanEquals:
	default:
		return false
	}

	p.lexer.ExpectLessThan(false /* isInsideJSXElement */)

	for {
		p.skipFlowType(js_ast.LLowest)
		if p.lexer.Token != js_lexer.TComma {
			break
		}
		p.lexer.Next()
	}

	// This type argument list must end with a ">"
	p.lexer.ExpectGreaterThan(isInsideJSXElement)
	return true
}

func (p *parser) trySkipFlowTypeArgumentsWithBacktracking() bool {
	oldLexer := p.lexer
	p.lexer.IsLogDisabled = true

	// Implement backtracking by restoring the lexer's memory to its original state
	defer func() {
		r := recover()
		if _, isLexerPanic := r.(js_lexer.LexerPanic); isLexerPanic {
			p.lexer = oldLexer
		} else if r != nil {
			panic(r)
		}
	}()

	p.skipFlowTypeArguments(false /* isInsideJSXElement */)

	// Check the token after this and backtrack if it's the wrong one
	if !p.canFollowTypeArgumentsInExpression() {
		p.lexer.Unexpected()
	}

	// Restore the log disabled flag. Note that we can't just set it back to false
	// because it may have been true to start with.
	p.lexer.IsLogDisabled = oldLexer.IsLogDisabled
	return true
}

func (p *parser) trySkipFlowTypeParametersThenOpenParenWithBacktracking() bool {
	oldLexer := p.lexer
	p.lexer.IsLogDisabled = true

	// Implement backtracking by restoring the lexer's memory to its original state
	defer func() {
		r := recover()
		if _, isLexerPanic := r.(js_lexer.LexerPanic); isLexerPanic {
			p.lexer = oldLexer
		} else if r != nil {
			panic(r)
		}
	}()

	p.skipFlowTypeParameters()
	if p.lexer.Token != js_lexer.TOpenParen {
		p.lexer.Unexpected()
	}

	// Restore the log disabled flag. Note that we can't just set it back to false
	// because it may have been true to start with.
	p.lexer.IsLogDisabled = oldLexer.IsLogDisabled
	return true
}

func (p *parser) trySkipFlowArrowReturnTypeWithBacktracking() bool {
	oldLexer := p.lexer
	p.lexer.IsLogDisabled = true

	// Implement backtracking by restoring the lexer's memory to its original state
	defer func() {
		r := recover()
		if _, isLexerPanic := r.(js_lexer.LexerPanic); isLexerPanic {
			p.lexer = oldLexer
		} else if r != nil {
			panic(r)
		}
	}()

	p.lexer.Expect(js_lexer.TColon)
	// We want to avoid parsing functions in arrow function
	p.fnOrArrowDataParse.isReturnType = true
	p.skipFlowReturnType()
	p.fnOrArrowDataParse.isReturnType = false

	// Check the token after this and backtrack if it's the wrong one
	if p.lexer.Token != js_lexer.TEqualsGreaterThan {
		p.lexer.Unexpected()
	}

	// Restore the log disabled flag. Note that we can't just set it back to false
	// because it may have been true to start with.
	p.lexer.IsLogDisabled = oldLexer.IsLogDisabled
	return true
}

func (p *parser) trySkipFlowArrowArgsWithBacktracking() bool {
	oldLexer := p.lexer
	p.lexer.IsLogDisabled = true

	// Implement backtracking by restoring the lexer's memory to its original state
	defer func() {
		r := recover()
		if _, isLexerPanic := r.(js_lexer.LexerPanic); isLexerPanic {
			p.lexer = oldLexer
		} else if r != nil {
			panic(r)
		}
	}()

	p.skipFlowFnArgs()
	p.lexer.Expect(js_lexer.TEqualsGreaterThan)

	// Restore the log disabled flag. Note that we can't just set it back to false
	// because it may have been true to start with.
	p.lexer.IsLogDisabled = oldLexer.IsLogDisabled
	return true
}

func (p *parser) skipFlowInterfaceStmt(opts parseStmtOpts) {
	name := p.lexer.Identifier
	p.lexer.Expect(js_lexer.TIdentifier)

	if opts.isModuleScope {
		p.localTypeNames[name] = true
	}

	p.skipFlowTypeParameters()

	if p.lexer.Token == js_lexer.TExtends {
		p.lexer.Next()
		for {
			p.skipFlowType(js_ast.LLowest)
			if p.lexer.Token != js_lexer.TComma {
				break
			}
			p.lexer.Next()
		}
	}

	if p.lexer.IsContextualKeyword("implements") {
		p.lexer.Next()
		for {
			p.skipFlowType(js_ast.LLowest)
			if p.lexer.Token != js_lexer.TComma {
				break
			}
			p.lexer.Next()
		}
	}

	p.skipFlowObjectType()
}

func (p *parser) skipFlowTypeStmt(opts parseStmtOpts) {
	if opts.isExport && p.lexer.Token == js_lexer.TOpenBrace {
		// "export type {foo}"
		// "export type {foo} from 'bar'"
		p.parseExportClause()
		if p.lexer.IsContextualKeyword("from") {
			p.lexer.Next()
			p.parsePath()
		}
		p.lexer.ExpectOrInsertSemicolon()
		return
	}

	name := p.lexer.Identifier
	p.lexer.Expect(js_lexer.TIdentifier)

	if opts.isModuleScope {
		p.localTypeNames[name] = true
	}

	p.skipFlowTypeParameters()
	p.lexer.Expect(js_lexer.TEquals)
	p.skipFlowType(js_ast.LLowest)
	p.lexer.ExpectOrInsertSemicolon()
}

func (p *parser) parseFlowDecorators() []js_ast.Expr {
	var tsDecorators []js_ast.Expr
	if p.options.flow.Parse {
		for p.lexer.Token == js_lexer.TAt {
			p.lexer.Next()

			// Parse a new/call expression with "exprFlagTSDecorator" so we ignore
			// EIndex expressions, since they may be part of a computed property:
			//
			//   class Foo {
			//     @foo ['computed']() {}
			//   }
			//
			// This matches the behavior of the Flow compiler.
			tsDecorators = append(tsDecorators, p.parseExprWithFlags(js_ast.LNew, exprFlagTSDecorator))
		}
	}
	return tsDecorators
}

//func (p *parser) parseFlowEnumStmt(loc logger.Loc, opts parseStmtOpts) js_ast.Stmt {
//	p.lexer.Expect(js_lexer.TEnum)
//	nameLoc := p.lexer.Loc()
//	nameText := p.lexer.Identifier
//	p.lexer.Expect(js_lexer.TIdentifier)
//	name := js_ast.LocRef{Loc: nameLoc, Ref: js_ast.InvalidRef}
//	argRef := js_ast.InvalidRef
//	if !opts.isFlowDeclare {
//		name.Ref = p.declareSymbol(js_ast.SymbolTSEnum, nameLoc, nameText)
//		p.pushScopeForParsePass(js_ast.ScopeEntry, loc)
//	}
//	p.lexer.Expect(js_lexer.TOpenBrace)

//	values := []js_ast.EnumValue{}

//	for p.lexer.Token != js_lexer.TCloseBrace {
//		value := js_ast.EnumValue{
//			Loc: p.lexer.Loc(),
//			Ref: js_ast.InvalidRef,
//		}

//		// Parse the name
//		if p.lexer.Token == js_lexer.TStringLiteral {
//			value.Name = p.lexer.StringLiteral()
//		} else if p.lexer.IsIdentifierOrKeyword() {
//			value.Name = js_lexer.StringToUTF16(p.lexer.Identifier)
//		} else {
//			p.lexer.Expect(js_lexer.TIdentifier)
//		}
//		p.lexer.Next()

//		// Identifiers can be referenced by other values
//		if !opts.isFlowDeclare && js_lexer.IsIdentifierUTF16(value.Name) {
//			value.Ref = p.declareSymbol(js_ast.SymbolOther, value.Loc, js_lexer.UTF16ToString(value.Name))
//		}

//		// Parse the initializer
//		if p.lexer.Token == js_lexer.TEquals {
//			p.lexer.Next()
//			value.ValueOrNil = p.parseExpr(js_ast.LComma)
//		}

//		values = append(values, value)

//		if p.lexer.Token != js_lexer.TComma && p.lexer.Token != js_lexer.TSemicolon {
//			break
//		}
//		p.lexer.Next()
//	}

//	if !opts.isFlowDeclare {
//		// Avoid a collision with the enum closure argument variable if the
//		// enum exports a symbol with the same name as the enum itself:
//		//
//		//   enum foo {
//		//     foo = 123,
//		//     bar = foo,
//		//   }
//		//
//		// Flow generates the following code in this case:
//		//
//		//   var foo;
//		//   (function (foo) {
//		//     foo[foo["foo"] = 123] = "foo";
//		//     foo[foo["bar"] = 123] = "bar";
//		//   })(foo || (foo = {}));
//		//
//		// Whereas in this case:
//		//
//		//   enum foo {
//		//     bar = foo as any,
//		//   }
//		//
//		// Flow generates the following code:
//		//
//		//   var foo;
//		//   (function (foo) {
//		//     foo[foo["bar"] = foo] = "bar";
//		//   })(foo || (foo = {}));
//		//
//		if _, ok := p.currentScope.Members[nameText]; ok {
//			// Add a "_" to make tests easier to read, since non-bundler tests don't
//			// run the renamer. For external-facing things the renamer will avoid
//			// collisions automatically so this isn't important for correctness.
//			argRef = p.newSymbol(js_ast.SymbolHoisted, "_"+nameText)
//			p.currentScope.Generated = append(p.currentScope.Generated, argRef)
//		} else {
//			argRef = p.declareSymbol(js_ast.SymbolHoisted, nameLoc, nameText)
//		}

//		p.popScope()
//	}

//	p.lexer.Expect(js_lexer.TCloseBrace)

//	if opts.isFlowDeclare {
//		if opts.isNamespaceScope && opts.isExport {
//			p.hasNonLocalExportDeclareInsideNamespace = true
//		}

//		return js_ast.Stmt{Loc: loc, Data: &js_ast.SFlow{}}
//	}

//	return js_ast.Stmt{Loc: loc, Data: &js_ast.SEnum{
//		Name:     name,
//		Arg:      argRef,
//		Values:   values,
//		IsExport: opts.isExport,
//	}}
//}
