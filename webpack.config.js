const path = require('path');
const outputDir = path.join(__dirname, "build/");
const BitBarWebpackProgressPlugin = require("bitbar-webpack-progress-plugin");

const isProd = process.env.NODE_ENV === 'production';

module.exports = {
  entry: './src/Index.bs.js',
  mode: isProd ? 'production' : 'development',
  output: {
    path: outputDir,
    publicPath: outputDir,
    filename: 'Index.js',
  },
  plugins: [
    new BitBarWebpackProgressPlugin()
  ]
};
