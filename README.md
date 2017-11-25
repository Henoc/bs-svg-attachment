# bs-svg-attachment

Tiny utility functions for SVG DOM mainly in Buckle Script / Reason.

## Features

- Simple, easy.
- No dirty DOM. No append some extra attributes.

## Installaion

SVG attachment depents on types of `bs-webapi`.

```bash
npm install bs-svg-attachment bs-webapi
```

Then add them to bs-dependencies in your bsconfig.json.

```json
{
  "name": "some",
  "source": "src",
  "bs-dependencies": [
    "bs-svg-attachment",
    "bs-webapi"
  ]
}
```
