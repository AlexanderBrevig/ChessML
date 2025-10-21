#!/usr/bin/env fish
# Download all opening PGN files from pgnmentor.com

set OPENINGS_DIR (pwd)/openings
mkdir -p $OPENINGS_DIR

echo "📥 Downloading ALL opening PGN files from pgnmentor.com..."
echo ""

# Base URL for pgnmentor openings
set BASE_URL "https://www.pgnmentor.com/openings"

# All opening databases from pgnmentor.com
# Organized by category: Modern Queen Pawn, Classical Queen Pawn, Modern King Pawn, Classical King Pawn, Flank & Unorthodox
set OPENINGS \
    "Modern.zip:Modern Defense" \
    "SemiBenoni.zip:Semi-Benoni" \
    "Trompowsky2Ne4.zip:Trompowsky 2...Ne4" \
    "Trompowsky2e6.zip:Trompowsky 2...e6" \
    "TrompowskyOther.zip:Trompowsky Other" \
    "Torre2e6.zip:Torre Attack 2...e6" \
    "Torre2g6.zip:Torre Attack 2...g6" \
    "London2e6.zip:London System 2...e6" \
    "London2g6.zip:London System 2...g6" \
    "Catalan3Bb4.zip:Catalan 3...Bb4+" \
    "Catalan3c5.zip:Catalan 3...c5" \
    "CatalanOpen.zip:Catalan Open" \
    "CatalanClosed.zip:Catalan Closed" \
    "BlackKnightTango.zip:Black Knights Tango" \
    "BudapestGambit.zip:Budapest Gambit" \
    "OldIndian.zip:Old Indian" \
    "CzechBenoni.zip:Czech Benoni" \
    "BenkoGambit.zip:Benko Gambit" \
    "ModernBenoni6Nf3.zip:Modern Benoni 6.Nf3" \
    "ModernBenoni6e4.zip:Modern Benoni 6.e4" \
    "DutchLeningrad.zip:Dutch Leningrad" \
    "DutchClassical.zip:Dutch Classical" \
    "Dutch3Nc3.zip:Dutch 3.Nc3" \
    "DutchOther.zip:Dutch Other" \
    "GrunfeldFianchetto.zip:Grunfeld Fianchetto" \
    "Grunfeld4Nf3.zip:Grunfeld 4.Nf3" \
    "GrunfeldOther.zip:Grunfeld Other" \
    "GrunfeldExchange.zip:Grunfeld Exchange" \
    "Bogo4Bd2.zip:Bogo-Indian 4.Bd2" \
    "Bogo4Nbd2.zip:Bogo-Indian 4.Nbd2" \
    "QID4a3.zip:Queen's Indian 4.a3" \
    "QID4Nc3.zip:Queen's Indian 4.Nc3" \
    "QID4e3.zip:Queen's Indian 4.e3" \
    "QIDOther.zip:Queen's Indian Other" \
    "QID4g3-Ba6.zip:Queen's Indian 4.g3 Ba6" \
    "QID4g3Other.zip:Queen's Indian 4.g3 Other" \
    "Nimzo4Nf3.zip:Nimzo-Indian 4.Nf3" \
    "NimzoSaemisch.zip:Nimzo-Indian Saemisch" \
    "NimzoLeningrad.zip:Nimzo-Indian Leningrad" \
    "Nimzo4f3.zip:Nimzo-Indian 4.f3" \
    "NimzoOther4.zip:Nimzo-Indian Other 4th moves" \
    "NimzoCl4O-O.zip:Nimzo-Indian Classical 4...O-O" \
    "NimzoCl4c5.zip:Nimzo-Indian Classical 4...c5" \
    "NimzoClOther.zip:Nimzo-Indian Classical Other" \
    "NimzoRub4O-O.zip:Nimzo-Indian Rubinstein 4...O-O" \
    "NimzoRub4c5.zip:Nimzo-Indian Rubinstein 4...c5" \
    "NimzoRubOther.zip:Nimzo-Indian Rubinstein Other" \
    "KIDAverbakh.zip:King's Indian Averbakh" \
    "KIDFianchetto.zip:King's Indian Fianchetto" \
    "KID4pawns.zip:King's Indian 4 Pawns Attack" \
    "KIDSaemisch.zip:King's Indian Saemisch" \
    "KIDClassical.zip:King's Indian Classical Main Line" \
    "KIDOther7.zip:King's Indian Classical Other 7th moves" \
    "KIDPetrosian.zip:King's Indian Classical Petrosian" \
    "KIDOther56.zip:King's Indian Classical Other 5th/6th moves" \
    "Hodgson.zip:Hodgson Attack" \
    "Colle.zip:Colle System" \
    "QGSym-Baltic.zip:QG Symmetrical and Baltic" \
    "QG-Albin.zip:QG Albin Counter-Gambit" \
    "QG-Chigorin.zip:QG Chigorin" \
    "SlavOther34.zip:Slav Other 3rd/4th moves" \
    "SlavExchange.zip:Slav Exchange" \
    "Slav4a6.zip:Slav 4...a6" \
    "SlavOther5.zip:Slav Other 5th moves" \
    "SlavMain.zip:Slav Main Line" \
    "QGA3e4.zip:Queen's Gambit Accepted 3.e4" \
    "QGAOther3.zip:QGA Other 3rd moves" \
    "QGAOther4.zip:QGA Other 4th moves" \
    "QGAMain.zip:QGA Main Line" \
    "QGDOther34.zip:QGD Other 3rd/4th moves" \
    "QGDTarrasch.zip:QGD Tarrasch" \
    "QGDExchange.zip:QGD Exchange" \
    "QGD5Bf4.zip:QGD 5.Bf4" \
    "SemiTarr5e3-Nc6.zip:QGD Semi-Tarrasch 5.e3 Nc6" \
    "SemiTarraschMain.zip:QGD Semi-Tarrasch Main" \
    "SemiSlavOther5.zip:QGD Semi-Slav Other 5th moves" \
    "SemiSlavBotvinnik.zip:QGD Semi-Slav Botvinnik" \
    "SemiSlavMeran.zip:QGD Semi-Slav Meran/Anti-Meran" \
    "QGDOrthoOther4.zip:QGD Orthodox Other 4th moves" \
    "QGDOrthoMain.zip:QGD Orthodox Main Line" \
    "OwenDefense.zip:Owen Defense" \
    "NimzoDefense.zip:Nimzowitsch Defense" \
    "Scand3Qd6-Qd8.zip:Scandinavian 3...Qd6/Qd8" \
    "Scand2Qxd5-3Qa5.zip:Scandinavian 3...Qa5" \
    "Scand2Nf6-3d4.zip:Scandinavian 2...Nf6 3.d4" \
    "Scand2Nf6Other.zip:Scandinavian 2...Nf6 Other" \
    "Alekhine2Nc3-d5.zip:Alekhine 2.Nc3 d5" \
    "AlekhineOther3.zip:Alekhine Other 3rd moves" \
    "AlekhineExchange.zip:Alekhine Exchange" \
    "Alekhine4Pawns.zip:Alekhine Four Pawns" \
    "AlekhineModern.zip:Alekhine Modern" \
    "Modern3Nf3.zip:Modern 3.Nf3" \
    "Modern3Nc3-d6.zip:Modern 3.Nc3 d6" \
    "Modern3Nc3-c6.zip:Modern 3.Nc3 c6" \
    "Modern3Nc3Other.zip:Modern 3.Nc3 Other" \
    "ModernOther3.zip:Modern Other 3rd moves" \
    "PircOtherWhite3.zip:Pirc Other White 3rd" \
    "PircOtherBlack3.zip:Pirc Other Black 3rd" \
    "PircAustrian.zip:Pirc Austrian Attack" \
    "PircOtherWhite4.zip:Pirc Other White 4th" \
    "PircClassical.zip:Pirc Classical" \
    "Caro-Kann2c4.zip:Caro-Kann 2.c4" \
    "Caro-Kann2Knight.zip:Caro-Kann Two Knights" \
    "Caro-KannEx.zip:Caro-Kann Exchange" \
    "Caro-KannPan-Bot.zip:Caro-Kann Panov-Botvinnik" \
    "Caro-KannAdv.zip:Caro-Kann Advance" \
    "Caro-Kann4Nf6.zip:Caro-Kann 4...Nf6" \
    "Caro-Kann4Nd7.zip:Caro-Kann 4...Nd7" \
    "Caro-KannClassic.zip:Caro-Kann Classical" \
    "FrenchKIA.zip:French King's Indian Attack" \
    "FrenchOther2.zip:French Other 2nd moves" \
    "FrenchExchange.zip:French Exchange" \
    "FrenchSchlechter.zip:French Schlechter" \
    "FrenchAdvance.zip:French Advance" \
    "FrTarraschOther3.zip:French Tarrasch Other 3rd" \
    "FrTarrasch3c5.zip:French Tarrasch 3...c5" \
    "FrTarrasch3Nf6.zip:French Tarrasch 3...Nf6" \
    "FrenchRubinstein.zip:French Rubinstein" \
    "FrWinawerOtherW4.zip:French Winawer Other White 4th" \
    "FrWinawerOtherB4.zip:French Winawer Other Black 4th" \
    "FrWinawerMain.zip:French Winawer Main Line" \
    "FrenchSteinitz.zip:French Steinitz" \
    "FrenchMacCutcheon.zip:French MacCutcheon" \
    "FrenchBurn.zip:French Burn" \
    "FrenchClassical.zip:French Classical" \
    "SicilianMisc2.zip:Sicilian Miscellaneous 2nd moves" \
    "Sicilian2f4.zip:Sicilian 2.f4" \
    "SicilianSmith-Morra.zip:Sicilian Smith-Morra" \
    "SicilianAlapinOther2.zip:Sicilian Alapin Other 2nd" \
    "SicilianAlapin2Nf6.zip:Sicilian Alapin 2...Nf6" \
    "SicilianAlapin2d5.zip:Sicilian Alapin 2...d5" \
    "Sicilian2Nc3-e6.zip:Sicilian 2.Nc3 e6" \
    "Sicilian2Nc3-d6.zip:Sicilian 2.Nc3 d6" \
    "SicilianGrandPrix.zip:Sicilian Grand Prix" \
    "SicilianClosedOther3.zip:Sicilian Closed Other 3rd" \
    "SicilianClosedMain.zip:Sicilian Closed Main" \
    "Sicilian2Nf3Other2.zip:Sicilian 2.Nf3 Other 2nd" \
    "SicilianRossolimo.zip:Sicilian Rossolimo" \
    "Sic2Nc6-4Qc7-4Qb6.zip:Sicilian 2...Nc6 4...Qc7/Qb6" \
    "SicilianLowenthal.zip:Sicilian Lowenthal" \
    "Sicilian2Nc6Other5.zip:Sicilian 2...Nc6 Other 5th" \
    "SicilianAccelDragon.zip:Sicilian Accelerated Dragon" \
    "SicilianSveshnikov.zip:Sicilian Sveshnikov" \
    "SicilianKanOther5.zip:Sicilian Kan Other 5th" \
    "SicilianKan5c4.zip:Sicilian Kan 5.c4" \
    "SicilianKan5Nc3.zip:Sicilian Kan 5.Nc3" \
    "SicilianKan5Bd3.zip:Sicilian Kan 5.Bd3" \
    "SicilianTaimanovOther5.zip:Sicilian Taimanov Other 5th" \
    "SicilianTaimanov5Nb5.zip:Sicilian Taimanov 5.Nb5" \
    "SicilianTaimanovMain.zip:Sicilian Taimanov Main" \
    "SicilianMoscow.zip:Sicilian Moscow" \
    "Sicilian2d6-4Qxd4.zip:Sicilian 2...d6 4.Qxd4" \
    "SicilianDragonOther6.zip:Sicilian Dragon Other 6th" \
    "SicDragon6Be2-6Bc4.zip:Sicilian Dragon 6.Be2/6.Bc4" \
    "SicilianDragonYugoslav.zip:Sicilian Dragon Yugoslav" \
    "SicilianClassicalOther6.zip:Sicilian Classical Other 6th" \
    "SicilianClassicalSozin.zip:Sicilian Classical Sozin" \
    "SicilianRichter-Rauzer.zip:Sicilian Richter-Rauzer" \
    "SicilianScheveningen.zip:Sicilian Scheveningen" \
    "SicilianNajdorf6a4.zip:Sicilian Najdorf 6.a4" \
    "SicilianNajdorf6g3.zip:Sicilian Najdorf 6.g3" \
    "SicilianNajdorf6f3.zip:Sicilian Najdorf 6.f3" \
    "SicilianNajdorf6f4.zip:Sicilian Najdorf 6.f4" \
    "SicilianNajdorf6Bc4.zip:Sicilian Najdorf 6.Bc4" \
    "SicilianNajdorf6Be3.zip:Sicilian Najdorf 6.Be3" \
    "SicilianNajdorf6Be2.zip:Sicilian Najdorf 6.Be2" \
    "SicilianNajdorf6Bg5.zip:Sicilian Najdorf 6.Bg5" \
    "CenterGame-Danish.zip:Center Game/Danish" \
    "BishopsOpening.zip:Bishop's Opening" \
    "KingsGambit.zip:King's Gambit" \
    "Vienna.zip:Vienna Game" \
    "Latvian-Elephant.zip:Latvian/Elephant Gambit" \
    "Philidor.zip:Philidor Defense" \
    "PetroffOther3.zip:Petroff Other 3rd" \
    "PetroffMain.zip:Petroff Main Line" \
    "Ponziani.zip:Ponziani" \
    "ThreeKnights.zip:Three Knights" \
    "FourKnights.zip:Four Knights" \
    "GoringGambit.zip:Goring Gambit" \
    "ScotchGambit.zip:Scotch Gambit" \
    "ScotchOther4.zip:Scotch Other 4th" \
    "Scotch4Nf6.zip:Scotch 4...Nf6" \
    "Scotch4Bc5.zip:Scotch 4...Bc5" \
    "Hungarian.zip:Hungarian Defense" \
    "TwoKnights.zip:Two Knights Defense" \
    "GiuocoPiano.zip:Giuoco Piano" \
    "RuyLopezOther3.zip:Ruy Lopez Other 3rd" \
    "RuyLopezClassical.zip:Ruy Lopez Classical" \
    "RuyLopezSchliemann.zip:Ruy Lopez Schliemann" \
    "RuyLopezBerlin.zip:Ruy Lopez Berlin" \
    "RuyLopezExchange.zip:Ruy Lopez Exchange" \
    "RuyLopezModSteinitz.zip:Ruy Lopez Modern Steinitz" \
    "RuyLopezOther5.zip:Ruy Lopez Other 5th" \
    "RuyLopezOpen.zip:Ruy Lopez Open" \
    "RuyMoeller-SteinDef.zip:Ruy Lopez Moeller/Steinitz Deferred" \
    "RuyLopezArchangelsk.zip:Ruy Lopez Archangelsk" \
    "RuyLopezOther6.zip:Ruy Lopez Other 6th" \
    "RuyLopezAntiMarshall.zip:Ruy Lopez Anti-Marshall" \
    "RuyLopezMarshall.zip:Ruy Lopez Marshall" \
    "RuyLopezOther9.zip:Ruy Lopez Other 9th" \
    "RuyLopezKar-Smy-Khol.zip:Ruy Lopez Karpov/Smyslov/Kholmov" \
    "RuyLopezBreyer.zip:Ruy Lopez Breyer" \
    "RuyLopezFlohr-Zaitsev.zip:Ruy Lopez Flohr-Zaitsev" \
    "RuyLopezChigorin.zip:Ruy Lopez Chigorin" \
    "English1b6.zip:English 1...b6" \
    "English1f5.zip:English 1...f5" \
    "English1c6.zip:English 1...c6" \
    "English1g6.zip:English 1...g6" \
    "EngSymDoubleFianchetto.zip:English Symmetrical Double Fianchetto" \
    "EnglishSymHedgehog.zip:English Symmetrical Hedgehog" \
    "EnglishSym3d4.zip:English Symmetrical 3.d4" \
    "EnglishSymOtherB3.zip:English Symmetrical Other Black 3rd" \
    "EnglishSymFourKnights.zip:English Symmetrical Four Knights" \
    "EnglishSymMain.zip:English Symmetrical Main" \
    "English1e6-2Nf3-d5.zip:English 1...e6 2.Nf3 d5" \
    "English1e6-2Nc3-d5.zip:English 1...e6 2.Nc3 d5" \
    "EnglishFlohr-Mikenas.zip:English Flohr-Mikenas" \
    "English1e6Main.zip:English 1...e6 Main" \
    "EnglishSicRev2g3.zip:English Sicilian Reversed 2.g3" \
    "EnglishSicRevOtherB2.zip:English Sicilian Reversed Other Black 2nd" \
    "EnglishSicRev4Knights.zip:English Sicilian Reversed Four Knights" \
    "EnglishSicRevBremen.zip:English Sicilian Reversed Bremen" \
    "EnglishSicRevClosed.zip:English Sicilian Reversed Closed" \
    "English1Nf6-2g3.zip:English 1...Nf6 2.g3" \
    "English1Nf6-2Nf3.zip:English 1...Nf6 2.Nf3 Anglo-Indian" \
    "English1Nf6-2Nc3.zip:English 1...Nf6 2.Nc3 Anglo-Indian" \
    "Reti2b3.zip:Reti 2.b3" \
    "Reti2c4.zip:Reti 2.c4" \
    "RetiKIA.zip:Reti King's Indian Attack" \
    "Bird.zip:Bird Opening" \
    "Nimzowitsch-Larsen.zip:Nimzowitsch-Larsen Attack" \
    "Sokolsky.zip:Sokolsky Opening" \
    "Dunst.zip:Dunst Opening"

set DOWNLOADED 0
set FAILED 0

for item in $OPENINGS
    set opening_file (echo $item | cut -d: -f1)
    set description (echo $item | cut -d: -f2)
    
    echo "⬇️  $description ($opening_file)"
    
    if curl -f -L -o "$OPENINGS_DIR/$opening_file" "$BASE_URL/$opening_file" 2>/dev/null
        set DOWNLOADED (math $DOWNLOADED + 1)
        echo "   ✅ Downloaded"
    else
        set FAILED (math $FAILED + 1)
        echo "   ❌ Failed"
    end
    echo ""
end

echo ""
echo "═══════════════════════════════════════════"
echo "Summary:"
echo "  ✅ Downloaded: $DOWNLOADED files"
echo "  ❌ Failed: $FAILED files"
echo "  📁 Location: $OPENINGS_DIR"
echo ""

# Unzip all files
echo "📦 Extracting PGN files..."
for zipfile in $OPENINGS_DIR/*.zip
    if test -f $zipfile
        unzip -q -o $zipfile -d $OPENINGS_DIR
        and echo "   ✅ Extracted "(basename $zipfile)
        or echo "   ❌ Failed to extract "(basename $zipfile)
    end
end

echo ""
echo "🎉 Done! PGN files are in: $OPENINGS_DIR"
ls -lh $OPENINGS_DIR/*.pgn 2>/dev/null | wc -l | read pgn_count
echo "   Found $pgn_count PGN files"
