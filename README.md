# cli-table-tool

Small util to convert file in an ascii table and convert it to csv.

This is tedius to do in Excel.

It is also a small Haskell starter project.

## Example input

E.g.

```
+---------------+--------+
|       Sentient|   count|
+---------------+--------+
|          Robot|    1628|
|Other Non-Human|    1738|
|         Monkey|   11258|
|   Space Aliens|  713280|
|      Smart-App| 8295982|
|          Human|43245298|
+---------------+--------+
```

## Example output

Should first be transformed to 

```
Sentient,count
Robot,1628
Other Non-Human,1738
Monkey,11258
Space Aliens,713280
Smart-App,8295982
Human,43245298
```

## Extenstion

It should detect what columns are numeric and calculate a percentage.
