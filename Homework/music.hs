import Data.List
type Name = String
data Song = Song Name
data Album = Album Name [Song] -- MusicianGroup
data Performer = Performer Name MusicianGroup
data MusicianGroup = MusicianGroup Name [Album] -- [Performer]

type Password = String
data User = User Name Password [Song]

getSongs :: MusicianGroup -> [Song]
getSongs (MusicianGroup _ albums ) = concat $ map(\(Album _ songs) -> songs) albums;

getTrackAuthor:: [MusicianGroup] -> Name -> Maybe MusicianGroup -- it doesn't work but i'll fix
getTrackAuthor [] _ = Nothing
getTrackAuthor (group:groups) name =  if find name (map (\(Song n) -> n)  $ getSongs MusicianGroup) /= Nothing then group else getTrackAuthor groups name   

songs = [ Song "Pozovi menya s soboi",Song "agent 007",Song "mama ne bei papu",Song "Serafim",Song "where are you",Song "santa"]
albums = [Album "leto 2018" [(songs !! 0), (songs !! 1)], Album "leto 2019" [(songs!! 2), ( songs!! 3)], Album "leto 2020" [(songs !! 4), (songs !! 5)]]
musicalGroups = [MusicianGroup "sirost" [albums !! 0, albums !! 1], MusicianGroup "sirost" [albums !! 2]]
