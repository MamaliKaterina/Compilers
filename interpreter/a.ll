; ModuleID = 'tony program'
source_filename = "tony program"

define i32 @simple_sum() {
entry:
  br label %return

return:                                           ; preds = %entry
  %c1 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %c2 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  %c3 = tail call i8* @malloc(i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32))
  store i8 97, i8* %c2
  store i8 98, i8* %c3
  ret i32 0
}

define void @test() {
entry:
  ret void
}

declare noalias i8* @malloc(i32)
