import Test.HUnit
import Dibujo
import Pred

-- Test case for cambiar function
testCambiar :: Test
testCambiar = TestCase $
    let inputDibujo = Juntar 1.0 2.0 (Figura 1) (Figura 2)             
        predCondition x = x == 2                                    
        transform x = Figura (x * 10)                                
        expectedDibujo = Juntar 1.0 2.0 (Figura 1) (Figura 20)       
        actualDibujo = cambiar predCondition transform inputDibujo   
    in assertEqual "cambiar should change elements based on predicate" expectedDibujo actualDibujo

-- Test case for anyDib function
testAnyDib :: Test
testAnyDib = TestCase $
    let inputDibujo = Juntar 1 2 (Figura 1) (Figura 2)           
        predCondition x = x == 2                                    
        expectedResult = True                                      
        actualResult = anyDib predCondition inputDibujo             
    in assertEqual "anyDib should identify if any element meets the predicate" expectedResult actualResult

-- Test case for allDib function
testAllDib :: Test
testAllDib = TestCase $
    let inputDibujo = Juntar 1 2 (Figura 1) (Figura 2)             
        predCondition x = x < 3                                     
        expectedResult = True                                      
        actualResult = allDib predCondition inputDibujo              
    in assertEqual "allDib should identify if all elements meet the predicate" expectedResult actualResult

-- Test case for andP function
testAndP :: Test
testAndP = TestCase $
    let p1 x = x > 0 && x < 10                                     
        p2 = even                               
        combinedP = andP p1 p2                                      
        expectedResult = True                                      
        actualResult = combinedP 4                         
    in assertEqual "andP should perform logical AND between predicates" expectedResult actualResult

-- Test case for orP function
testOrP :: Test
testOrP = TestCase $
    let p1 x = x < 0                                            
        p2  = even                                     
        combinedP = orP p1 p2 
        expectedResult = True                                    
        actualResult = combinedP 4                                   
    in assertEqual "orP should perform logical OR between predicates" expectedResult actualResult

-- Grouping tests
tests :: Test
tests = TestList[
                    TestLabel "testCambiar" testCambiar,
                    TestLabel "testAnyDib" testAnyDib,
                    TestLabel "testAllDib" testAllDib,
                    TestLabel "testAndP" testAndP,
                    TestLabel "testOrP" testOrP
                ]

-- Running the tests
main :: IO ()
main = do
    counts <- runTestTT tests
    putStrLn $ "Test results: " ++ show counts