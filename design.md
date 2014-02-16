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


Should metrics have a numeric ID or a (semi?) readable string-based ID.  If they're numeric, we need a way to assign IDs, which seems complicated. IDs could be 5 (?) char strings (e.g 'tempf', 'humid', etc). This works to some moderate scale, but still probably doesn't handle multiple, completely independent people creating sensors (e.g. one person could implement 'humid' as a number between 0 and 1 and someone else between 0 and 100).  Alternatively we could have namespaces, but this is just going to make things longer and uglier. It's not clear how efficient we even we even want to be with these names, if collection intervals are low, and there aren't many sensors then saying "com.foo.bar.Humidity" is fine ... but this would be wasteful to collect as networks grow.

A much more complicated solution is to negotiate metric IDs.  For example a sensor and relay can negotiate that "com.foo.bar.Humidity" maps to "42" for their connection. This allows is to use highly qualified names with namespaces for actually identifying metrics, but only using small ints in the wire protocol. However, it's a more complicated protocol!

Strawman proposal:
- All metrics are identified by a fully qualified name (e.g. com.foo.bar.Humidity)
- There's an _optional_ Negotiation protocol, which allows any two nodes to map a full name to an int for efficiency.  This is a strictly local mapping  When talking to a different node, a separate negotiation must happen. (e.g. Sensor & Relay could negotiate that com.foo.bar.Humidity maps to 42, and Relay & Hub could negotiate that it maps to 43.  The midtier has to keep both mappings and use the correct number.
- When reporting measurements, it's valid to report using either the fully qualified name, or a numeric ID *if one has been negotiated*

Strawman protocol:

MAP(string fullyQualifiedMetricName) -> int32 metricId

REPORT(string fullyQualifiedMetricName, int64 value, int64 timestamp)

REPORT(int32 metricId, int64 value, int64 timestamp)

Questions:
- Same protocol sensor->relay as relay->hub?
- batch reporting?


Push vs. pull
-------------
Currently thinking it should be push for both (i.e. sensors push to midtiers, and midtiers can push to hub/storage).

Pros:

- Allows sensor nodes to sleep if they want
- Midtiers don't have to keep state of connected sensors

Cons:

- If midtiers don't know about sensors: they can't do health checking (same goes for storage nodes health checking the midtiers).


Discovery
---------
If sensor pushes to midtier, we need to teach sensors to discover midtiers.  Maybe midtiers just live on a "well known" PAN and address?
