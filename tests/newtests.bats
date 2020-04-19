load harness


@test "new_test_1" {
  check 'if y>=1 ∨ 4 <= 4 then y:= 1 else y:= 3' '{y → 1}'
}

@test "new_test_2" {
  check 'if 0<=x ∨ 6=5 then x := 1 else x:= 3' '{x → 1}'
}



