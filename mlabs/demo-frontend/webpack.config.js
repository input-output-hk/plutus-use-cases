'use strict';

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
const isWatch = process.argv.some(a => a === '--watch');


const plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

console.log (isWebpackDevServer || isWatch)
module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: path.resolve(__dirname, 'dist'),
    port: 8082,
    hot: true
  },

  entry: './dev/index.js',

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },

  mode: 'development',
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'src/*.purs',
                'src/**/*.purs'
              ],
              spago: true,
              watch: isWebpackDevServer || isWatch,
              pscIde: true
            }
          }
        ]
      },
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 8192,
            },
          },
        ],
      },
    ]
  },

  resolve: {
    modules: [ 'node_modules' ],
    extensions: [ '.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    }),
    new HtmlWebpackPlugin({
      title: 'purescript-webpack-example',
      template: './dev/index.html',
      inject: false  // See stackoverflow.com/a/38292765/3067181
    })
  ].concat(plugins)
};
