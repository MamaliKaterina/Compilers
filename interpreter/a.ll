; ModuleID = 'tony program'
source_filename = "tony program"

define i32 @main() {
entry:
  br label %return

return:                                           ; preds = %entry
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %limit = bitcast i8* %malloccall to i32*
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %number = bitcast i8* %malloccall1 to i32*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %counter = bitcast i8* %malloccall2 to i32*
  store i32 10, i32* %limit
  store i32 0, i32* %counter
  %loadtmp = load i32, i32* %limit
  %eqtmp = icmp sge i32 %loadtmp, 2
  %if_cond = icmp ne i1 %eqtmp, false
  br i1 %if_cond, label %then, label %else

then:                                             ; preds = %return
  %loadtmp3 = load i32, i32* %counter
  %addtmp = add i32 %loadtmp3, 1
  store i32 %addtmp, i32* %counter
  br label %after

else:                                             ; preds = %return
  br label %after

after:                                            ; preds = %else, %then
  %loadtmp4 = load i32, i32* %limit
  %eqtmp5 = icmp sge i32 %loadtmp4, 3
  %if_cond6 = icmp ne i1 %eqtmp5, false
  br i1 %if_cond6, label %then7, label %else8

then7:                                            ; preds = %after
  %loadtmp10 = load i32, i32* %counter
  %addtmp11 = add i32 %loadtmp10, 1
  store i32 %addtmp11, i32* %counter
  br label %after9

else8:                                            ; preds = %after
  br label %after9

after9:                                           ; preds = %else8, %then7
  store i32 6, i32* %number
  br label %loop

loop:                                             ; preds = %after31, %after9
  %loadtmp13 = load i32, i32* %number
  %loadtmp14 = load i32, i32* %limit
  %eqtmp15 = icmp sle i32 %loadtmp13, %loadtmp14
  %loop_cond = icmp ne i1 %eqtmp15, false
  br i1 %loop_cond, label %body, label %after12

body:                                             ; preds = %loop
  %loadtmp16 = load i32, i32* %number
  %subtmp = sub i32 %loadtmp16, 1
  %calltmp = call i1 @"prime?"(i32 %subtmp)
  %if_cond17 = icmp ne i1 %calltmp, false
  br i1 %if_cond17, label %then18, label %else19

after12:                                          ; preds = %loop
  ret i32 0

then18:                                           ; preds = %body
  %loadtmp21 = load i32, i32* %counter
  %addtmp22 = add i32 %loadtmp21, 1
  store i32 %addtmp22, i32* %counter
  br label %after20

else19:                                           ; preds = %body
  br label %after20

after20:                                          ; preds = %else19, %then18
  %loadtmp23 = load i32, i32* %number
  %loadtmp24 = load i32, i32* %limit
  %diftmp = icmp ne i32 %loadtmp23, %loadtmp24
  %loadtmp25 = load i32, i32* %number
  %addtmp26 = add i32 %loadtmp25, 1
  %calltmp27 = call i1 @"prime?"(i32 %addtmp26)
  %andtmp = and i1 %diftmp, %calltmp27
  %if_cond28 = icmp ne i1 %andtmp, false
  br i1 %if_cond28, label %then29, label %else30

then29:                                           ; preds = %after20
  %loadtmp32 = load i32, i32* %counter
  %addtmp33 = add i32 %loadtmp32, 1
  store i32 %addtmp33, i32* %counter
  br label %after31

else30:                                           ; preds = %after20
  br label %after31

after31:                                          ; preds = %else30, %then29
  %loadtmp34 = load i32, i32* %number
  %addtmp35 = add i32 %loadtmp34, 6
  store i32 %addtmp35, i32* %number
  br label %loop
}

define i1 @"prime?"(i32 %n1) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %n = bitcast i8* %malloccall to i32*
  store i32 %n1, i32* %n
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %i = bitcast i8* %malloccall2 to i32*
  %loadtmp = load i32, i32* %n
  %eqtmp = icmp slt i32 %loadtmp, 0
  %if_cond = icmp ne i1 %eqtmp, false
  br i1 %if_cond, label %then, label %else

then:                                             ; preds = %entry
  %loadtmp3 = load i32, i32* %n
  %negtmp = sub i32 0, %loadtmp3
  %calltmp = call i1 @"prime?"(i32 %negtmp)
  ret i1 %calltmp

after_return:                                     ; No predecessors!
  br label %after

else:                                             ; preds = %entry
  %loadtmp4 = load i32, i32* %n
  %eqtmp5 = icmp slt i32 %loadtmp4, 2
  %if_cond6 = icmp ne i1 %eqtmp5, false
  br i1 %if_cond6, label %new_then, label %else7

new_then:                                         ; preds = %else
  ret i1 false

after_return8:                                    ; No predecessors!
  br label %after

else7:                                            ; preds = %else
  %loadtmp9 = load i32, i32* %n
  %eqtmp10 = icmp eq i32 %loadtmp9, 2
  %if_cond11 = icmp ne i1 %eqtmp10, false
  br i1 %if_cond11, label %new_then12, label %else13

new_then12:                                       ; preds = %else7
  ret i1 true

after_return14:                                   ; No predecessors!
  br label %after

else13:                                           ; preds = %else7
  %loadtmp15 = load i32, i32* %n
  %modtmp = srem i32 %loadtmp15, 2
  %eqtmp16 = icmp eq i32 %modtmp, 0
  %if_cond17 = icmp ne i1 %eqtmp16, false
  br i1 %if_cond17, label %new_then18, label %else19

new_then18:                                       ; preds = %else13
  ret i1 false

after_return20:                                   ; No predecessors!
  br label %after

else19:                                           ; preds = %else13
  store i32 3, i32* %i
  br label %loop

after:                                            ; preds = %after_return35, %after_return20, %after_return14, %after_return8, %after_return
  unreachable

loop:                                             ; preds = %after32, %else19
  %loadtmp22 = load i32, i32* %i
  %loadtmp23 = load i32, i32* %n
  %divtmp = sdiv i32 %loadtmp23, 2
  %eqtmp24 = icmp sle i32 %loadtmp22, %divtmp
  %loop_cond = icmp ne i1 %eqtmp24, false
  br i1 %loop_cond, label %body, label %after21

body:                                             ; preds = %loop
  %loadtmp25 = load i32, i32* %n
  %loadtmp26 = load i32, i32* %i
  %modtmp27 = srem i32 %loadtmp25, %loadtmp26
  %eqtmp28 = icmp eq i32 %modtmp27, 0
  %if_cond29 = icmp ne i1 %eqtmp28, false
  br i1 %if_cond29, label %then30, label %else31

after21:                                          ; preds = %loop
  ret i1 true

after_return35:                                   ; No predecessors!
  br label %after

then30:                                           ; preds = %body
  ret i1 false

after_return33:                                   ; No predecessors!
  br label %after32

else31:                                           ; preds = %body
  br label %after32

after32:                                          ; preds = %else31, %after_return33
  %loadtmp34 = load i32, i32* %i
  %addtmp = add i32 %loadtmp34, 2
  store i32 %addtmp, i32* %i
  br label %loop
}

declare noalias i8* @malloc(i32)
