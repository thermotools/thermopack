---
layout: default
version: 2.2
title: Installing ThermoPack 2.2.0
permalink: /v2.2.0/source_build.html
---

## Version 2.2
ThermoPack version 2.2.0 implements all features found in the [latest version](/thermopack/) of ThermoPack, while retaining
backwards compatibility with v2.1.0. In order to retain backwards compatibility, there are some differences in 
the return patterns between v2.2.0 and the latest version. The differentiation is handled in the last step of the build process,
when running `makescript.py`.

## Building from source

In order to build ThermoPack v2.2 from source follow the [steps to build the latest version](/thermopack/vcurrent/source_build.html) 
until the step where `makescript.py` is run.

To build and install v2.2, run

```bash
python makescript.py optim -diffs=v2
pip install .
```

## After installing
When things are properly installed, it may be useful to look into the [getting started guide](/thermopack/v2.2.0/getting_started.html)
to see how to use the new features in v2.2.0.