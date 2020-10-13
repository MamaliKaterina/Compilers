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
  %malloccall3 = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 16))
  %arraytmp = bitcast i8* %malloccall3 to i32*
  store i32* %arraytmp, i32** %x
  store i32 65, i32* %seed
  store i32 0, i32* %i
  br label %loop

loop:                                             ; preds = %body, %entry
  %loadtmp = load i32, i32* %i
  %eqtmp = icmp slt i32 %loadtmp, 16
  %loop_cond = icmp ne i1 %eqtmp, false
  br i1 %loop_cond, label %body, label %after

body:                                             ; preds = %loop
  %loadtmp4 = load i32, i32* %seed
  %multmp = mul i32 %loadtmp4, 137
  %addtmp = add i32 %multmp, 220
  %loadtmp5 = load i32, i32* %i
  %addtmp6 = add i32 %addtmp, %loadtmp5
  %modtmp = srem i32 %addtmp6, 101
  store i32 %modtmp, i32* %seed
  %loadtmp7 = load i32, i32* %i
  %addtmp8 = add i32 %loadtmp7, 1
  store i32 %addtmp8, i32* %i
  br label %loop

after:                                            ; preds = %loop
  ret i32 0
}

declare noalias i8* @malloc(i32)
