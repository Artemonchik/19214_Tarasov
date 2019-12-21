import Data.List
type Name = String
data Song = Song Name deriving(Show)
data Album = Album Name [Song] deriving(Show)-- MusicianGroup
data Performer = Performer Name (Maybe MusicianGroup) deriving(Show)
data MusicianGroup = MusicianGroup Name [Album] deriving(Show) -- [Performer]

type Password = String
data User = User Name Password [Song]

getSongs :: MusicianGroup -> [Song]
getSongs (MusicianGroup _ albums ) = concat $ map(\(Album _ songs) -> songs) albums;

getTrackAuthor:: [MusicianGroup] -> Name -> Maybe MusicianGroup
getTrackAuthor [] _ = Nothing
getTrackAuthor groups name =  find hasSongInGroup groups where 
                            hasSongInAlbum (Album _ songs) = any (\(Song songName) -> name == songName) songs
                            hasSongInGroup (MusicianGroup _ albums) = any hasSongInAlbum albums

songs = [ Song "Pozovi menya s soboi",Song "agent 007",Song "mama ne bei papu",Song "Serafim",Song "where are you",Song "santa"]
albums = [Album "leto 2018" [(songs !! 0), (songs !! 1)], Album "leto 2019" [(songs!! 2), ( songs!! 3)], Album "leto 2020" [(songs !! 4), (songs !! 5)]]
musicalGroups = [MusicianGroup "sirost" [albums !! 0, albums !! 1], MusicianGroup "sirost" [albums !! 2]]
