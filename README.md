#Hack your own Erlang VM

## Logistics

My e-mail is rafal.studnicki(at)erlang-solutions.com.

If you have any questions concerning the exercises please don't hesitate to ask :)

If you manage to implement them please submit them by e-mail as well.

## Exercises

### 1.1 Implement atom table

The task is to code the atom table that stores atoms in an index table.
Please remember that we want to be able to check if an atom has already been put into the table
as well as fetch an atom using its index.

If you are using Erlang, please don't use atoms :-)

You can find the structure of the module's atom table below.

```
+------+--------+--------+--------+--------+
| byte |   0    |   1    |   2    |   3    |
+---------------+--------+--------+--------+
|  0   |   number of atoms in the table    |
+---------------+--------------------------+
|  4   |atom len|          atom 1          |
+---------------+--------------------------+
|  8   |                  payload          |
+---------------+--------------------------+
| ...  |atom len|          atom 2          |
+---------------+--------------------------+
| ...  |                  payload          |
+------------------------------------------+
```

### 1.2 Implement export table

The task is to code the export table that will store the
``(module, function, arity) -> function entrypoint`` relation.

This task is tightly coupled with 1.3.

As in 1.1, please don't use atoms if you are using Erlang.
Instead, use indices from the atom table that you've implemented.

The structure of a module's export table has been attached below.

```
+------------------------------------------+
| byte |   0    |   1    |   2    |   3    |
+---------------+--------+--------+--------+
|  0   |   number of exported functions    |
+------------------------------------------+
|  4   |    function 1 name atom index     |
+------------------------------------------+
|  8   |         function 1 arity          |
+------------------------------------------+
| 12   |    function 1 entrypoint label    |
+------------------------------------------+
| 16   |    function 2 name atom index     |
+------------------------------------------+
| ...  |                ...                |
+------------------------------------------+
```

### 1.3 Implement code table

The task is to implement the code table.
Ideally, it should be easy to find the code for a given module and to reference it in the exercise 1.2.

At this moment you don't have to worry about labels, opcodes and their arguments.
Simply put the whole bytecode into the table, starting from opcode 1.

You can find code chunk's structure below.

And yes, please use atom table indices only :-)

```
+------------------------------------------+
| byte |   0    |   1    |   2    |   3    |
+------------------------------------------+
|  0   |00000000|00000000|00000000|00010000|
+------------------------------------------+
|  4   |00000000|00000000|00000000|00000000|
+---------------+--------+--------+--------+
|  8   |     maximum opcode number used    |
+------------------------------------------+
| 12   |        number of labels used      |
+------------------------------------------+
| 16   |    number of exported functions   |
+---------------+--------------------------+
| 20   |opcode 1|        arguments...      |
+------------------------------------------+
| ...  |opcode 2|            ...           |
+------+--------+--------------------------+
```
