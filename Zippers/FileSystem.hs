import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

(-:) :: a -> (a -> b) -> b
x -: f = f x

myDisk :: FSItem
myDisk = 
  Folder "root"
  [ File "goat_yelling_like_man.wmv" "baaaaa"
  , File "pope_time.avi" "god bless"
  , Folder "pics"
    [ File "ape_throwing_crap.jpg" "bleargh"
    , File "watermelon_smash.gif" "smash!"
    , File "skull_man(scary).bmp" "Yikes!"
    ]
  , File "dijon_poupon.doc" "best mustard"
  , Folder "programs"
    [ File "fartwizard.exe" "10gotofart"
    , File "owl_bandit.dmg" "mov eax, hOOt"
    , File "not_a_virus.exe" "really not a virus"
    , Folder "source code"
      [ File "best_hs_prog.hs" "main = print (fix error)"
      , File "random.hs" "main = print 4" 
      ]
    ]
  ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

-- the zipper
type FSFocus = (FSItem, [FSCrumb])
fsUp :: FSFocus -> FSFocus
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSFocus -> FSFocus
fsTo name (Folder folderName items, bs) = 
  let (ls, item:rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _ ) = name == folderName
nameIs name (File fileName _) = name == fileName

-- manipulating the filesystem
fsRename :: Name -> FSFocus -> FSFocus
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSFocus -> FSFocus 
fsNewFile item (Folder folderName items, bs) =
  (Folder folderName (item:items), bs)
