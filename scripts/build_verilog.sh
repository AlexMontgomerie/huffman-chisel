#!/bin/bash
module=$1
code_table_path=$2
len_table_path=$3

inst=""

if [ "$module" == "encoder" ] ; then

    # create accum module instance
    inst="huffman.BufferedEncoder(UInt(8.W), \"${code_table_path}\", \"${len_table_path}\")"

elif [ "$module" == "decoder" ] ; then

    # create max pool module instance
    inst="huffman.BufferedDecoder(UInt(8.W), \"${code_table_path}\", \"${len_table_path}\")"

fi


# create file
cat <<EOF > _temphelper.scala
package _temphelper
import chisel3._
object Elaborate extends App {
  chisel3.Driver.execute(args, () => new ${inst})
}
EOF

# build verilog
sbt "runMain _temphelper.Elaborate --target-dir impl "

# clean up file
rm _temphelper.scala


