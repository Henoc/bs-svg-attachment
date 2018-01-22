# bs-svg-attachment

Tiny utility functions for SVG DOM mainly in BuckleScript / Reason.

## Features

- Simple, easy.
- No dirty DOM. No append some extra attributes.

## Installation

SVG attachment depends on types of `bs-webapi`.

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

## Modules

- Svg
  Functions for one element
- SvgSet
  Functions for element set
