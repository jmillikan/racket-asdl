#lang ragg

# Some of these clauses exist to 

modules: module*
module: "module" CONSTRUCTORID maybe_version "{" type* "}"
maybe_version: ["version" STRING]
type: TYPEID "=" constructor_list maybe_attributes
constructor_list: constructor ("|" constructor)* 
maybe_attributes: ["attributes" properties]
constructor: CONSTRUCTORID [properties]
properties: "(" [property ("," property)*] ")"
property: TYPEID maybe_modifier TYPEID
maybe_modifier: ["?" | "*"]


