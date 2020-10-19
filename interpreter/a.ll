; ModuleID = 'tony program'
source_filename = "tony program"

@string1 = global [18 x i8] c"\22Initial array: \22\00"
@string_ptr1 = global i8* getelementptr inbounds ([18 x i8], [18 x i8]* @string1, i32 0, i32 0)
@string2 = global [17 x i8] c"\22Sorted array: \22\00"
@string_ptr2 = global i8* getelementptr inbounds ([17 x i8], [17 x i8]* @string2, i32 0, i32 0)

define i32 @main() {
entry:
  br label %return

return:                                           ; preds = %entry
  br label %return1

return1:                                          ; preds = %return
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %seed = bitcast i8* %malloccall to i32*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall2 to i32*
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %x = bitcast i8* %malloccall3 to i32**
  %malloccall4 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 16))
  %arraytmp = bitcast i8* %malloccall4 to i32*
  store i32* %arraytmp, i32** %x
  store i32 65, i32* %seed
  store i32 0, i32* %i
  br label %loop

loop:                                             ; preds = %body, %return1
  %loadtmp = load i32, i32* %i
  %eqtmp = icmp slt i32 %loadtmp, 16
  %loop_cond = icmp ne i1 %eqtmp, false
  br i1 %loop_cond, label %body, label %after

body:                                             ; preds = %loop
  %loadtmp5 = load i32, i32* %seed
  %multmp = mul i32 %loadtmp5, 137
  %addtmp = add i32 %multmp, 220
  %loadtmp6 = load i32, i32* %i
  %addtmp7 = add i32 %addtmp, %loadtmp6
  %modtmp = srem i32 %addtmp7, 101
  store i32 %modtmp, i32* %seed
  %loadtmp8 = load i32, i32* %i
  %loadtmp9 = load i32*, i32** %x
  %ptr_to_int = ptrtoint i32* %loadtmp9 to i32
  %addptr = add i32 %ptr_to_int, %loadtmp8
  %int_to_ptr = inttoptr i32 %addptr to i32*
  %loadtmp10 = load i32, i32* %seed
  store i32 %loadtmp10, i32* %int_to_ptr
  %loadtmp11 = load i32, i32* %i
  %addtmp12 = add i32 %loadtmp11, 1
  store i32 %addtmp12, i32* %i
  br label %loop

after:                                            ; preds = %loop
  %loadtmp13 = load i8*, i8** @string_ptr1
  %loadtmp14 = load i32*, i32** %x
  call void @writeArray(i8* %loadtmp13, i32 16, i32* %loadtmp14)
  %loadtmp15 = load i32*, i32** %x
  call void @bsort(i32 16, i32* %loadtmp15)
  %loadtmp16 = load i8*, i8** @string_ptr2
  %loadtmp17 = load i32*, i32** %x
  call void @writeArray(i8* %loadtmp16, i32 16, i32* %loadtmp17)
  ret i32 0
}

define void @bsort(i32 %n1, i32* %x3) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %n = bitcast i8* %malloccall to i32*
  store i32 %n1, i32* %n
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %x = bitcast i8* %malloccall2 to i32**
  store i32* %x3, i32** %x
  br label %return

return:                                           ; preds = %entry
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall4 to i32*
  %malloccall5 = tail call i8* @malloc(i32 ptrtoint (i1* getelementptr (i1, i1* null, i32 1) to i32))
  %changed = bitcast i8* %malloccall5 to i1*
  store i1 true, i1* %changed
  br label %loop

loop:                                             ; preds = %after8, %return
  %loadtmp = load i1, i1* %changed
  %loop_cond = icmp ne i1 %loadtmp, false
  br i1 %loop_cond, label %body, label %after

body:                                             ; preds = %loop
  store i1 false, i1* %changed
  store i32 0, i32* %i
  br label %loop6

after:                                            ; preds = %loop
  ret void

loop6:                                            ; preds = %after22, %body
  %loadtmp9 = load i32, i32* %i
  %loadtmp10 = load i32, i32* %n
  %subtmp = sub i32 %loadtmp10, 1
  %eqtmp = icmp slt i32 %loadtmp9, %subtmp
  %loop_cond11 = icmp ne i1 %eqtmp, false
  br i1 %loop_cond11, label %body7, label %after8

body7:                                            ; preds = %loop6
  %loadtmp12 = load i32, i32* %i
  %loadtmp13 = load i32*, i32** %x
  %ptr_to_int = ptrtoint i32* %loadtmp13 to i32
  %addptr = add i32 %ptr_to_int, %loadtmp12
  %int_to_ptr = inttoptr i32 %addptr to i32*
  %loadtmp14 = load i32, i32* %int_to_ptr
  %loadtmp15 = load i32, i32* %i
  %addtmp = add i32 %loadtmp15, 1
  %loadtmp16 = load i32*, i32** %x
  %ptr_to_int17 = ptrtoint i32* %loadtmp16 to i32
  %addptr18 = add i32 %ptr_to_int17, %addtmp
  %int_to_ptr19 = inttoptr i32 %addptr18 to i32*
  %loadtmp20 = load i32, i32* %int_to_ptr19
  %eqtmp21 = icmp sgt i32 %loadtmp14, %loadtmp20
  %if_cond = icmp ne i1 %eqtmp21, false
  br i1 %if_cond, label %then, label %else

after8:                                           ; preds = %loop6
  br label %loop

then:                                             ; preds = %body7
  %loadtmp23 = load i32, i32* %i
  %loadtmp24 = load i32*, i32** %x
  %ptr_to_int25 = ptrtoint i32* %loadtmp24 to i32
  %addptr26 = add i32 %ptr_to_int25, %loadtmp23
  %int_to_ptr27 = inttoptr i32 %addptr26 to i32*
  %loadtmp28 = load i32, i32* %i
  %addtmp29 = add i32 %loadtmp28, 1
  %loadtmp30 = load i32*, i32** %x
  %ptr_to_int31 = ptrtoint i32* %loadtmp30 to i32
  %addptr32 = add i32 %ptr_to_int31, %addtmp29
  %int_to_ptr33 = inttoptr i32 %addptr32 to i32*
  call void @swap(i32* %int_to_ptr27, i32* %int_to_ptr33)
  store i1 true, i1* %changed
  br label %after22

else:                                             ; preds = %body7
  br label %after22

after22:                                          ; preds = %else, %then
  %loadtmp34 = load i32, i32* %i
  %addtmp35 = add i32 %loadtmp34, 1
  store i32 %addtmp35, i32* %i
  br label %loop6
}

declare noalias i8* @malloc(i32)

define void @swap(i32* %x, i32* %y) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %t = bitcast i8* %malloccall to i32*
  %loadtmp = load i32, i32* %x
  store i32 %loadtmp, i32* %t
  %loadtmp1 = load i32, i32* %y
  store i32 %loadtmp1, i32* %x
  %loadtmp2 = load i32, i32* %t
  store i32 %loadtmp2, i32* %y
  ret void
}

define void @writeArray(i8* %msg1, i32 %n3, i32* %x5) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %msg = bitcast i8* %malloccall to i8**
  store i8* %msg1, i8** %msg
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %n = bitcast i8* %malloccall2 to i32*
  store i32 %n3, i32* %n
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %x = bitcast i8* %malloccall4 to i32**
  store i32* %x5, i32** %x
  %malloccall6 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall6 to i32*
  store i32 0, i32* %i
  br label %loop

loop:                                             ; preds = %after10, %entry
  %loadtmp = load i32, i32* %i
  %loadtmp7 = load i32, i32* %n
  %eqtmp = icmp slt i32 %loadtmp, %loadtmp7
  %loop_cond = icmp ne i1 %eqtmp, false
  br i1 %loop_cond, label %body, label %after

body:                                             ; preds = %loop
  %loadtmp8 = load i32, i32* %i
  %eqtmp9 = icmp sgt i32 %loadtmp8, 0
  %if_cond = icmp ne i1 %eqtmp9, false
  br i1 %if_cond, label %then, label %else

after:                                            ; preds = %loop
  ret void

then:                                             ; preds = %body
  br label %after10

else:                                             ; preds = %body
  br label %after10

after10:                                          ; preds = %else, %then
  %loadtmp11 = load i32, i32* %i
  %addtmp = add i32 %loadtmp11, 1
  store i32 %addtmp, i32* %i
  br label %loop
}
