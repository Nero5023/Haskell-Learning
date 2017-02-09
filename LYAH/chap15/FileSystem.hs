import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:fs) = Just (Folder name $ rs ++ [item] ++ rs, fs)
fsUp (_, []) = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, fs) =
    let (ls, item:rs) = break (nameIs name) items
    in Just (item, FSCrumb folderName ls rs:fs)
fsTo _ _ = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, fs) = (Folder newName items, fs)
fsRename newName (File name d, fs)    = (File newName d, fs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, fs) = 
    (Folder folderName $ item:items, fs)
