import Test.HUnit
import Dibujo

-- Test cases for construction functions
testComp :: Test
testComp = TestCase (assertEqual "comp 0 should return the input" (comp 0 (+1) 5) 5)

testFigura :: Test
testFigura = TestCase (assertEqual "figura should return a Figura" (figura 5) (Figura 5))

testEncimar :: Test
testEncimar = TestCase (assertEqual "encimar should return an Encimar" (encimar (Figura 5) (Figura 6)) (Encimar (Figura 5) (Figura 6)))

testApilar :: Test
testApilar = TestCase (assertEqual "apilar should return an Apilar" (apilar 5 6 (Figura 5) (Figura 6)) (Apilar 5 6 (Figura 5) (Figura 6)))

testJuntar :: Test
testJuntar = TestCase (assertEqual "juntar should return a Juntar" (juntar 5 6 (Figura 5) (Figura 6)) (Juntar 5 6 (Figura 5) (Figura 6)))

testRot45 :: Test
testRot45 = TestCase (assertEqual "rot45 should return a Rot45" (rot45 (Figura 5)) (Rot45 (Figura 5)))

testRotar :: Test
testRotar = TestCase (assertEqual "rotar should return a Rotar" (rotar (Figura 5)) (Rotar (Figura 5)))

testEspejar :: Test
testEspejar = TestCase (assertEqual "espejar should return an Espejar" (espejar (Figura 5)) (Espejar (Figura 5)))

-- Test cases for combinators
testEncimarComb :: Test
testEncimarComb = TestCase (assertEqual "^^^ should return an Encimar" ((Figura 5) ^^^ (Figura 6)) (Encimar (Figura 5) (Figura 6)))

testApilarComb :: Test
testApilarComb = TestCase (assertEqual ".-. should return an Apilar" ((Figura 5) .-. (Figura 6)) (Apilar 1.0 1.0 (Figura 5) (Figura 6)))

testJuntarComb :: Test
testJuntarComb = TestCase (assertEqual "/// should return a Juntar" ((Figura 5) /// (Figura 6)) (Juntar 1.0 1.0 (Figura 5) (Figura 6)))

-- Test cases for rotations
testR90 :: Test
testR90 = TestCase (assertEqual "r90 should return three times Rotar" (r90 (Figura 5)) (Rotar (Figura 5)))

testR180 :: Test
testR180 = TestCase (assertEqual "r180 should return two times Rotar" (r180 (Figura 5)) (Rotar (Rotar (Figura 5))))

testR270 :: Test
testR270 = TestCase (assertEqual "r270 should return three times Rotar" (r270 (Figura 5)) (Rotar (Rotar (Rotar (Figura 5)))))

-- Test cases for complex rotations
testEncimar4 :: Test
testEncimar4 = TestCase $
    let inputDibujo = Figura "test"  
        expectedDibujo = Encimar (Figura "test") (Encimar (Rotar (Figura "test")) (Encimar (Rotar (Rotar (Figura "test"))) (Rotar (Rotar (Rotar (Figura "test"))))))
        actualDibujo = encimar4 inputDibujo
    in assertEqual "encimar4 should apply ^^^ with rotations correctly" expectedDibujo actualDibujo

testCuarteto :: Test
testCuarteto = TestCase $
    let d1 = Figura "circle"
        d2 = Figura "square"
        d3 = Figura "triangle"
        d4 = Figura "star"
        expectedDibujo = (d1 /// d2) .-. (d3 /// d4)
        actualDibujo = cuarteto d1 d2 d3 d4           
    in assertEqual "cuarteto should combine dibujos correctly" expectedDibujo actualDibujo

testCiclar :: Test
testCiclar = TestCase $
    let inputDibujo = Figura "circle"
        expectedDibujo = cuarteto inputDibujo (r90 inputDibujo) (r180 inputDibujo) (r270 inputDibujo)  
        actualDibujo = ciclar inputDibujo
    in assertEqual "ciclar should apply cuarteto with rotations correctly" expectedDibujo actualDibujo

-- Test case for map function
testMap :: Test
testMap = TestCase $
    let inputDibujo = Figura 5
        expectedDibujo = Figura 10
        actualDibujo = mapDib (*2) inputDibujo
    in assertEqual "mapDib should apply the function to the dibujo" expectedDibujo actualDibujo

-- Test case for fold function
testFoldDib :: Test
testFoldDib = TestCase $
    let inputDibujo = Figura "circle"
        expectedValue = "circle"
        actualValue = foldDib
            (\a -> a)                    
            (\b -> "Rotar " ++ b)       
            (\b -> "Espejar " ++ b)     
            (\b -> "Rot45 " ++ b)       
            (\f1 f2 b1 b2 -> "Apilar " ++ show f1 ++ " " ++ show f2 ++ " (" ++ b1 ++ ") (" ++ b2 ++ ")")  
            (\f1 f2 b1 b2 -> "Juntar " ++ show f1 ++ " " ++ show f2 ++ " (" ++ b1 ++ ") (" ++ b2 ++ ")")   
            (\b1 b2 -> "Encimar (" ++ b1 ++ ") (" ++ b2 ++ ")")   
            inputDibujo
    in assertEqual "foldDib should transform Figura correctly" expectedValue actualValue

-- Test case for change function
testChange :: Test
testChange = TestCase $
    let inputDibujo = Figura "circle"             
        expectedDibujo = Figura "changed circle"        
        f x = Figura ("changed " ++ x)                   
        actualDibujo = change f inputDibujo             
    in assertEqual "change should apply transformation f correctly" expectedDibujo actualDibujo

-- Grouping test cases
tests :: Test
tests = TestList [TestLabel "testComp" testComp,
                  TestLabel "testFigura" testFigura,
                  TestLabel "testEncimar" testEncimar,
                  TestLabel "testApilar" testApilar,
                  TestLabel "testJuntar" testJuntar,
                  TestLabel "testRot45" testRot45,
                  TestLabel "testRotar" testRotar,
                  TestLabel "testEspejar" testEspejar,
                  TestLabel "testEncimarComb" testEncimarComb,
                  TestLabel "testApilarComb" testApilarComb,
                  TestLabel "testJuntarComb" testJuntarComb,
                  TestLabel "testR90" testR90,
                  TestLabel "testR180" testR180,
                  TestLabel "testR270" testR270,
                  TestLabel "testEncimar4" testEncimar4,
                  TestLabel "testCuarteto" testCuarteto,
                  TestLabel "testCiclar" testCiclar,
                  TestLabel "testMap" testMap,
                  TestLabel "testFoldDib" testFoldDib,
                  TestLabel "testChange" testChange]

-- Running the tests
main :: IO ()
main = do
    counts <- runTestTT tests
    putStrLn $ "Test results: " ++ show counts