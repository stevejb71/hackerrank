import Search

main :: IO ()
main = do
  _ : strings <- lines <$> getContents
  let answers = yesOrNo <$> containsAll strings
  mapM_ putStrLn answers

yesOrNo :: Bool -> String
yesOrNo True = "YES"
yesOrNo False = "NO"
