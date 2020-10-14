; ModuleID = 'tony program'
source_filename = "tony program"

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %seed = bitcast i8* %malloccall to i32*
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall1 to i32*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %x = bitcast i8* %malloccall2 to i32**
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %b = bitcast i8* %malloccall3 to i32**
  %malloccall4 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 16))
  %arraytmp = bitcast i8* %malloccall4 to i32*
  store i32* %arraytmp, i32** %x
  store i32 65, i32* %seed
  %loadtmp = load i32*, i32** %x
  %ptr_to_int = ptrtoint i32* %loadtmp to i32
  %addptr = add i32 %ptr_to_int, 5
  %int_to_ptr = inttoptr i32 %addptr to i32*
  store i32 1, i32* %int_to_ptr
  store i32 0, i32* %i
  br label %loop

loop:                                             ; preds = %body, %entry
  %loadtmp5 = load i32, i32* %i
  %eqtmp = icmp slt i32 %loadtmp5, 16
  %loop_cond = icmp ne i1 %eqtmp, false
  br i1 %loop_cond, label %body, label %after

body:                                             ; preds = %loop
  %loadtmp6 = load i32, i32* %seed
  %multmp = mul i32 %loadtmp6, 137
  %addtmp = add i32 %multmp, 220
  %loadtmp7 = load i32, i32* %i
  %addtmp8 = add i32 %addtmp, %loadtmp7
  %modtmp = srem i32 %addtmp8, 101
  store i32 %modtmp, i32* %seed
  %loadtmp9 = load i32, i32* %i
  %addtmp10 = add i32 %loadtmp9, 1
  store i32 %addtmp10, i32* %i
  br label %loop

after:                                            ; preds = %loop
  ret i32 0
}

declare noalias i8* @malloc(i32)
