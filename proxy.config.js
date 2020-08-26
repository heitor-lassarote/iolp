const proxy = [
    {
        context: "/api",
        target: "https://b0a90d100dd1.ngrok.io",
        pathRewrite: { "^/api": "" },
    },
];

module.exports = proxy;
