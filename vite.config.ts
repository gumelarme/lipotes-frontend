import fs from 'fs'
import dotenv from 'dotenv'
import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

dotenv.config({path: `.env.${process.env.APP_ENV ?? 'dev'}`})

const createElmFile = (moduleName: string, vars: {[key: string]: string}) => {
    // TODO: make it typed
    const preamble = `module ${moduleName} exposing (..)`
    const sortedVars = Object.entries(vars)
        .sort((a, b) => a[0].localeCompare(b[0]))
        .map(([k, v]) => {
            const varName = k.toLowerCase()
            return [
                `${varName}: String`,
                `${varName} = "${v}"`,
            ].join("\n")
        })
        .join("\n\n");


    const target = `src/${moduleName}.elm`;
    try {
        fs.writeFileSync(target, [preamble, sortedVars].join("\n\n"))
        console.log(`${target} file created.`)
    } catch (err) {
        console.error(`Failed to generate ${target} file from environment`)
        throw err
    }
}

const generateElmEnvPlugin = (options: {prefix: string}) => ({
    name: "generate-elm-env",
    buildStart(opt){
        const prefix = options.prefix ?? "HZM_"
        const filtered = Object.entries(process.env).filter(([k, v]) => k.startsWith(prefix))
        const envs = Object.fromEntries(filtered);
        createElmFile("Env", envs);
    }
})

export default defineConfig({
  plugins: [
      generateElmEnvPlugin({prefix: "HZM_"}),
      elmPlugin.default()
  ],
});


