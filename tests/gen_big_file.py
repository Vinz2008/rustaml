


out_file = "big_file.rml"

f = open(out_file, mode = "w")


for i in range(0, 1000):
    f.writelines(
        ["let f{} a =".format(i), 
         "\tif a == {} then".format(i),
         "\t\ta * 6 + 3" 
         "\telse f{} a - 1 ;;\n\n".format(i)]
    )

f.close()