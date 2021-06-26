package main

import (
	"os"

	"github.com/lpalmes/esbuild/pkg/api"
)

func main() {
	result := api.Build(api.BuildOptions{
		EntryPoints: []string{"/Users/lpalmes/workspace/web/apps/responsive-web/src/app/client/index.js"},
		Format:      api.FormatESModule,
		Bundle:      true,
		Outdir:      "compiled",
		Splitting:   true,
		Plugins:     []api.Plugin{},
		Loader: map[string]api.Loader{
			".js":      api.LoaderFlow,
			".png":     api.LoaderDataURL,
			".jpg":     api.LoaderDataURL,
			".jpeg":    api.LoaderDataURL,
			".svg":     api.LoaderText,
			".graphql": api.LoaderText,
		},
		Write:    true,
		LogLevel: api.LogLevelError,
		LogLimit: 10,
	})

	if len(result.Errors) > 0 {
		os.Exit(1)
	}
}
