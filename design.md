Three Tiered System
===================
1. Leaf/Sensor nodes
2. Midtier/Relay nodes
3. Hub/Storage nodes

(Names need more thought)

Protocols
=========
1. From leaves to midtiers, custom protocol over XBee
2. From midtiers to hubs, JSON (?) over HTTP

Open Design Issues
==================
- New leaf discovery
- Direction of push/pull


Detailed Design
===============
(In Progress)

"Collection Protocol"
---------------------
Between midtier and leaves.

Needs to contain:

- leaf node id
- list of metrics, each with an identifier and value
- timestamp?
- collect historical values? or just current value?

Needs to be simple to generate (generated on tiny microcontrollers), implementable from scratch with no libraries (i.e. no Protos).


