const path = require("path");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const dist = path.resolve(__dirname, "static", "dist");

module.exports = {
    mode: "production",
    entry: {
        index: "./src-js/main.js"
    },
    output: {
        path: dist,
        filename: "[name].js"
    },
    devServer: {
        static: path.resolve(__dirname, "static"),
    },
    plugins: [
        new WasmPackPlugin({
            crateDirectory: __dirname,
        }),
    ],
    experiments: {
        asyncWebAssembly: true,
    }
};