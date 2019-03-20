const path = require('path');
const WasmPackPlugin = require('@wasm-tool/wasm-pack-plugin');

module.exports = {
  configureWebpack: {
    plugins: [
      new WasmPackPlugin({
        crateDirectory: path.resolve(__dirname, '..')
        // WasmPackPlugin defaults to compiling in "dev" profile. To change that, use forceMode: 'release':
        // forceMode: 'release'
      })
    ]
  }
};
