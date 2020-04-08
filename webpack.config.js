var path = require('path');

module.exports = {
    module: {
        rules: [{
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.mp3$/,
                exclude: /node_modules/,
                loader: 'file-loader',
                options: {
                    name: '[folder]/[name].[ext]'
                }
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "elm-webpack-loader",
                options: {
                    debug: true
                }
            },
            {
                test: /\.scss$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false", "sass-loader"]
            }
        ]
    },

    devServer: {
        inline: true,
        stats: 'errors-only'
    }
};