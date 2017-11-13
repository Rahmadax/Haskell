module Test where 

	testFun :: Int -> Bool
	testFun a
	  | (a == 1) = True
	  | (a == 2) = True
	testFun 0 = False
