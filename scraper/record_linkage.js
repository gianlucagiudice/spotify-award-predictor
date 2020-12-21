db.data.createIndex({"name": 1})
db.award.createIndex({"title": 1})



// Find unique songs in data
db.data.aggregate([
    {
        $group: {
            "_id": { name: "$name", artists: "$artists" },
            valence : {$first: "$valence"},
            year : {$first: "$year"},
            acousticness : {$first: "$acousticness"},
            danceability : {$first: "$danceability"},
            duration_ms : {$first: "$duration_ms"},
            energy : {$first: "$energy"},
            explicit : {$first: "$explicit"},
            id : {$first: "$id"},
            instrumentalness : {$first: "$instrumentalness"},
            key : {$first: "$key"},
            liveness : {$first: "$liveness"},
            loudness : {$first: "$loudness"},
            mode : {$first: "$mode"},
            popularity : {$first: "$popularity"},
            release_date : {$first: "$release_date"},
            speechiness : {$first: "$speechiness"},
            tempo : {$first: "$tempo"}
        }
    },
    {
        $project: {
            "_id": 0,
            "name": "$_id.name",
            "artists": "$_id.artists",
            "valence": "$valence",
            "year": "$year",
            "acousticness": "$acousticness",
            "danceability": "$danceability",
            "duration_ms": "$duration_ms",
            "energy": "$energy",
            "explicit": "$explicit",
            "id": "$id",
            "instrumentalness": "$instrumentalness",
            "key": "$key",
            "liveness": "$liveness",
            "loudness": "$loudness",
            "mode": "$mode",
            "popularity": "$popularity",
            "release_date": "$release_date",
            "speechiness": "$speechiness",
            "tempo": "$tempo",
        }
    },
    {
        $out: "data"
    } 
],
    {allowDiskUse: true}
).pretty()



// Convert actors field to list
db.data.find().forEach(function(doc){
    let artists = doc["artists"];
    artists_parsed = artists.replace(/^./, '').replace(/.$/, '').split(',')

    artists = []
    artists_parsed.forEach(artist => {
        trimmed = artist.trim().replace(/^./, '').replace(/.$/, '')
        //print(trimmed)
        artists.push(trimmed)
    })
    
    doc["artists"] = artists

    db.data.save(doc)
})



// Convert actors field to list
db.awards.find().forEach(function(doc){
    artists = String(doc["artists"]);
    
    artists_parsed = artists.split(',')

    artists = []
    artists_parsed.forEach(artist => {
        trimmed = artist.trim()
        artists.push(trimmed)
    })
    
    doc["artists"] = artists

    db.awards.save(doc)

})



db.data.createIndex({"artists": 1})
db.award.createIndex({"artists": 1})



// Songs with awards
db.data.aggregate([
    {
        $lookup:
            {
                from: "awards",
                let: {"song_name": "$name", "artists": "$artists"},
                pipeline: [
                    {
                        $match: {
                            $expr: {
                                $eq: ["$title", "$$song_name"]
                            }
                        }
                    }

                ],
                as: "award"
        }
    },
    {
        $out: "joined"
    }
])



// Unwind artists
db.joined.aggregate([
    {
        $project: {
            award: {$first: "$award"},
            "name": "$name",
            "artists": "$artists",
            "valence": "$valence",
            "year": "$year",
            "acousticness": "$acousticness",
            "danceability": "$danceability",
            "duration_ms": "$duration_ms",
            "energy": "$energy",
            "explicit": "$explicit",
            "id": "$id",
            "instrumentalness": "$instrumentalness",
            "key": "$key",
            "liveness": "$liveness",
            "loudness": "$loudness",
            "mode": "$mode",
            "popularity": "$popularity",
            "release_date": "$release_date",
            "speechiness": "$speechiness",
            "tempo": "$tempo",
        }
    },
    {
        $out: "joined"
    }
])



// Unwind artists
db.joined.update(
    {
        'award': {$exists : false}
    },
    {
        $set: {
            "award_type": null,
            "award_nation": null,
        }
    },
    { multi: true }
)



db.joined.createIndex({"name": 1})
db.joined.createIndex({"artists": 1})



// Songs with awards
db.joined.aggregate([
    {
        $unwind: {path: "$award.artists"}
    },

    {
        $match: {
            $expr: {
                $in: ["$award.artists", "$artists"]
            }
        }
    },
    {
        $project: {
            "name": "$name",
            "artists": "$artists",
            "valence": "$valence",
            "year": "$year",
            "acousticness": "$acousticness",
            "danceability": "$danceability",
            "duration_ms": "$duration_ms",
            "energy": "$energy",
            "explicit": "$explicit",
            "id": "$id",
            "instrumentalness": "$instrumentalness",
            "key": "$key",
            "liveness": "$liveness",
            "loudness": "$loudness",
            "mode": "$mode",
            "popularity": "$popularity",
            "release_date": "$release_date",
            "speechiness": "$speechiness",
            "tempo": "$tempo",
            "award_type": "$award.award",
            "award_nation": "$award.nation",
        }
    },
    {
        $group: {
            "_id": { name: "$name", artists: "$artists" },
            valence : {$first: "$valence"},
            year : {$first: "$year"},
            acousticness : {$first: "$acousticness"},
            danceability : {$first: "$danceability"},
            duration_ms : {$first: "$duration_ms"},
            energy : {$first: "$energy"},
            explicit : {$first: "$explicit"},
            id : {$first: "$id"},
            instrumentalness : {$first: "$instrumentalness"},
            key : {$first: "$key"},
            liveness : {$first: "$liveness"},
            loudness : {$first: "$loudness"},
            mode : {$first: "$mode"},
            popularity : {$first: "$popularity"},
            release_date : {$first: "$release_date"},
            speechiness : {$first: "$speechiness"},
            tempo : {$first: "$tempo"},

            award_type: {$first: "$award_type"},
            award_nation: {$first: "$award_nation"},
        }
    },
    {
        $project: {
            "_id": 0,
            "name": "$_id.name",
            "artists": "$_id.artists",
            "valence": "$valence",
            "year": "$year",
            "acousticness": "$acousticness",
            "danceability": "$danceability",
            "duration_ms": "$duration_ms",
            "energy": "$energy",
            "explicit": "$explicit",
            "id": "$id",
            "instrumentalness": "$instrumentalness",
            "key": "$key",
            "liveness": "$liveness",
            "loudness": "$loudness",
            "mode": "$mode",
            "popularity": "$popularity",
            "release_date": "$release_date",
            "speechiness": "$speechiness",
            "tempo": "$tempo",
            "award_type": "$award_type",
            "award_nation": "$award_nation",
        }
    },
    {
        $out: "joined_award"
    }
]).pretty()



db.joined.aggregate([
    {
        $match: {
            award: {$exists: false}
        }
    },
    {
        $unionWith: { coll: "joined_award"}
    },
    {
        $out: "songs"
    }
])



db.songs.find().forEach(function(doc){
    artists = doc['artists']
    joined = artists.join(',')
    doc['artists'] = joined
    db.songs.save(doc)
})