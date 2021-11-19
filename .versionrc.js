module.exports = {
    bumpFiles: [
        {
            filename: "package.json",
            type: "json"
        },
        {
            filename: "modules/pipeline/package.json",
            type: "json"
        },
        {
            filename: "modules/render/package.json",
            type: "json"
        },
        {
            filename: "modules/version/package.json",
            type: "json"
        },
        {
            filename: "modules/website/package.json",
            type: "json"
        },
    ],
    header: 'Changelog',
    types: [
        {type: 'feat', section: "Features"},
        {type: 'fix', section: "Bug Fixes"},
    ]
}
