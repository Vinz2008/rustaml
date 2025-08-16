


out_file = "big_file.rml"

f = open(out_file, mode = "w")


for i in range(0, 10000):
    f.writelines(
        ["let f{} a{} =".format(i, i), 
         "\tif a{} == {} then".format(i, i),
         "\t\ta{} * 6 + 3".format(i), 
         "\telse f{} a{} - 1 ;;\n\n".format(i, i)]
    )

f.close()