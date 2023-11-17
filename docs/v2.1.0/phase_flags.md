---
layout: default
version: 2.1.0
title: Phase flags
permalink: /v2.1.0/phase_flags.html
---

The phase keys are defined in src/thermopack_constants.f90, and are stored as attributes of the `thermo` class. That is, the phase flag for liquid (`1`) can be accessed as `eos.LIQPH` from any equation of state object. See [Getting started](/thermopack/v2.1.0/getting_started.html) for an introduction on how to initialise an equation of state.

| Phase           | Value  | Code name  | Description |
| --------------- | ------ | ---------- | ----------- |
| Two-phase       |    0   | TWOPH      | Liquid-vapor two-phase mixture |
| Liquid          |    1   | LIQPH      | Single phase liquid  |
| Vapor           |    2   | VAPPH      | Single phase vapor  |
| Minimum Gibbs   |    3   | MINGIBBSPH | Single phase root with the minimum Gibbs free energy |
| Single          |    4   | SINGLEPH   | Single phase not identified as liquid or vapor |
| Solid           |    5   | SOLIDPH    | Single phase solid |
| Fake            |    6   | FAKEPH     | In rare cases no physical roots exist, and a fake liquid root is returned |
