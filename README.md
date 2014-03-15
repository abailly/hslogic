# Introduction

A logic programming based framework to allocate resources on systems.

The goal of this tool is to allow defining machines and environments provisioning in a declarative way based on
[linear logic](https://en.wikipedia.org/wiki/Linear_logic) programming. Linear logic is a *logic of resources* where logical
propositions use in a proof can be restricted thus supporting the concept of *consuming*  resources to *produce*  results. Linear
logic also embeds classical (or intuitionistic) logic formulas so it is possible to express properties that hold under resources
constraints or abstract truths.

As of now it mostly tries to implement a Prolog like interpreter understanding linear logic constructs.
