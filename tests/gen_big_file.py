


out_file = "big_file.rml"

f = open(out_file, mode = "w")


for i in range(0, 10000):
    f.writelines(
        ["let f{} a{} =".format(i, i), 
         "\tif a{} == {} then".format(i, i),
         "\t\ta{} * 6 + 3".format(i), 
         "\telse f{} a{} - 1 ;;\n\n".format(i, i),
         "let m{} b{} = match b{} with".format(i, i, i),
         "\t| 3 -> \"test\"",
         "\t| 0..1 -> \"other test\"",
         "\t| n -> \"last\" ;; \n\n",
         ]
    )

f.close()