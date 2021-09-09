#!/bin/bash

DIR1="../tony_programs/testing_error_catching"
DIR2="../tony_programs/testing_functionality"
files1=`ls ${DIR1}/*.tony`
files2=`ls ${DIR2}/*.tony`

for test_program in $files1 $files2
do
  echo "Press enter to check next program."
  read -n1 key
  echo "Compiling test program: ./do.sh ${test_program}"
  ./tony $test_program
  if [ $? -eq 0 ]; then
    name=${test_program%.tony}
    name=${name##*/} 
    echo "Running test program: ./$name.out"
    ./$name.out
  fi
  echo "---------------------------------------------------------------------"
  echo ""
done
