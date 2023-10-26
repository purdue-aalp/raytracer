## FU usage summary for Baseline Raytracer

| Stage | FU used by Raytriangle | FU used by Raybox |
| --- | --- | --- |
| 1 | NA | NA |
|2 | NA | NA |
|3|9 Adders|**24 Adders**|
|4*|9 MulAdds | **24 Multipliers(as MulAdd)**|
|5|**6 Multipliers**| 0 | 
|6|**3 Adders**|0|
|7|**3 Multipliers**|0|
|8|**2 Adders**|0|
|9|**2 Adders**|0|
|10*|**5 Compare**|**12 CAS, 8 min/max**, 4 Compare, **2 QuadSort**|


## FU usage summary for Enhanced Raytracer

| Stage | FU used by Raytriangle | FU used by Raybox | FU used by Euclidian |
| ---   | ---   | ---   | --- | 
|1      | NA    | NA    | NA |
|2      | NA    | NA    | NA |
|3      |9 Adds     |**24 Adds**  | 16 Adds |
|4      |9 Muls     | **24 Muls** | 16 Muls |
|4 1/2  |**9 Adds** | 0           | 8 Adds  |
|5      |**6 Muls** | 0           |0        |
|6      |  3 Adds   |0            |**4 Adds**|
|7      |**3 Muls** |0            |0        |
|8      |**2 Adds** |0            |2 Adds   |
|9      |**2 Adds** |0            |1 Add    |
|10     |**5 Compare**|**12 CAS, 8 min/max**, 4 Compare|**1 Add**| 
|10 1/2 |0          |2 QuadSort   |0        |