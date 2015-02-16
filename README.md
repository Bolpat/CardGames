# CardGames

## Greetings

Hello Everyone! Please consider that I'm creating this as a graded project
and I don't want you to help me until I've held the presentation.
The presentation will be on 2015-03-25.
After this date, I'd find it great someone collaborating.
Espacially for graphics I'm not that firm with.

## Watten - A Quick Overview

Watten is a famous card game in Bavaria, some parts of Austria and what I've heard also in South Tyrol.

You usually play with 2, 3 or 4 players, but it is also possible with 5 or 6.
Here you can choose the number you prefer.

### Aim

The players get 5 cards each.
You try to get the most tricks in a round - at least 3.
If you have partners, your tricks will be added up.
The team with the most tricks wins the round and the round's value is added to the players' scores.
Usually you play until one player's score reaches 11, 15 or 21.
Then this player wins the game.

### Teams

For 2 players there are no teams.  
3 players have separate scores, but for every round, the dealer and the player who cut the cards are in one team.  
In 4 player games, players sitting opposite one another build a fixed team.  
For 5 players there is no common rule. Here it is implemented, that the dealer and the player after the dealer build one team.  
For 6 players, players who sit next to each other or opposite one another are opponents.

### Bidding

In every case the king of hearts is the highest card in the game, then secondly the seven of bells and third the seven of acorns.
Theese three ones are called crits ("Kritische" in German).
The first player after the dealer is the one to choose an arbitrary rank.
The cards which have this ranks are called "Schläge" (sg. "Schlag) and will beat any other card except the crits.
After that, dealer chooses any suit which is trump ("Trumpf" in German).
Every card that has this trump suit beats any card with no given role.

### Trick taking

The trumps are ordered. Higher ranks beat lower ones.
One of the "Schläge" has the trump suit; this one is called the "Hauptschlag".
It is the fourth best card, directly after the crits.
The other "Schläge" are equal and when colliding the first one to be played beats other ones.
A card that neither is a trump nor beats trumps can be beaten by trumps or cards beating trumps of corse,
but otherwise only by a card of the same suit when its rank is higher.

The first player after the dealer starts the first trick.
The players play one card each one after another until every player has played one card.
The trick taking player collects the cards and places them face down.
If there are cards in their hand, this player starts the next trick.

Usually a player is allowed to play any cards in their hand, with one exception:
Only in the first trick, if the first played card is the "Hauptschlag", then any player MUST play a trump card or a crit;
if both is not possible, the player may play any card.
Once a crit is played by another player, the exception is void for following players.

### Score

At the beginning every round is worth 2 points.
In your turn, you can "ausschaffen".
Then the opposing players have to decide if they want to give up.
If they don't give up, the round is continued and worth one more point.

At the beginning of each round any team may do the first "Ausschaffen", but then it has to alternate.
If you used "ausschaffen" and your opponents choose not to do, you can't increse the round's value.

When a player has a score of 9, 13 or 19 he/she is called "gespannt" and may not "ausschaffen". If they do, they lose 2 points of their score.
In a game with fixed teams, when a team is "gespannt", every round is started with an "Ausschaffen" by the lower team, if they are not "gespannt" either.

# Kartenspiele

## Grüße

Hallo ihr alle! Bitte beachtet, dass das hier als benotetes Uni-Projekt entsteht und ich keine Hilfe von außen annehmen darf,
zumindest bis zur Präsentation am 25.3.
Danach würde ich mich durchaus über Zusammenarbeit freuen, insbesondere bei Grafik, wo ich nicht so fit bin.

## Watten - Übersicht der Regeln

Watten Kartenspiel, das in Bayern, Österreich und - wie mir zugetragen wurde - auch in Südtirol.

Es wird mit 2 bis 4 Spielern gespielt.
Da ich auch Regeln für 5 und 6 Spieler entwickelt habe, stehen dise Anzahlen auch zur Verfügung.
In der Implementierung ist die Anzahl zu Beginn wählbar.

### Ziel des Spiels

Jeder Spieler erhält 5 Karten.
Die beiden Parteien versuchen von den 5 möglichen Stichen mindestens 3 zu ergattern;
diese gewinnt dann die Runde und wird den Spielern gutgeschrieben.
Es wird so lange gespielt, bis ein Spieler 11, 15 oder 21 Punkte erreicht.

### Parteien

Für 2 Spieler kann es keine sinnvollen Parteien geben; jeder spielt für sich.  
Bei 3 Spielern hat jeder seine eigenen Punkte. Im jeder Runde spielt Vorhand allein gegen die anderen beiden.  
Bei 4 Spielern spielen gegenübersitzende Spieler zusammen.  
Für 5 Spieler gibt es keine einheitlichen Regeln. Hier ist es so, dass Geber und Vorhand gegen die anderen spielen.  
Bei 6 Spielern sind nebeneinander oder gegenüber sitzende Spieler Gegner.

### Ansagen

In jedem Fall sind Herz-König, Schellen-Sieben und Eichel-Sieben die höchsten Karten im Spiel, diese heißen "Kritische".
Vorhand wählt einen Rang, den "Schalg", für diese Runde; Karten, die diesen Rang haben heißen "Schläge".
Dann wählt der Geber eine Farbe, den "Trumpf", für diese Runde; Karten, die diese Farbe haben heißen "Trümpfe".

### Stechen

Die eindeutige Karte, die durch Schlag und Trumpf bezeichnet wird, ist der "Hauptschlag" und
ist (falls sie nicht kritisch ist) nach den Kritischen die vierthöchste Karte in der Runde.
Dann folgen gleichberechtigt alle anderen Schläge. Kollidieren gleiche Schläge, so gewinnt der erste.
Die Trümpfe sind durch ihren Rang geordnet. Karten, die keine Trümpfe, Schläge oder Kritische sind,
können außer mit ebenjenen nur mit Karten derselben Farbe und höherem Rang gestochen werden.

Vorhand spielt die erste Karte aus. Die übrigen Spieler geben reihum je eine Karte zu bis jeder eine Karte gespielt hat.
Der Spieler, dessen Karte den Stich gewinnt, nimmt den Stich und platziert ihn verdeckt vor sich.
Falls er noch Karten hat, spielt dieser Spieler zum nächsten Sptich aus.

Normalerweise darf jede Karte ausgespielt oder zugegeben werden, jedoch gibt es eine Ausnahme:
Im ersten Stich der Runde, falls die erste Karte der Hauptschlag ist, gilt die Regel "Trumpf oder Kritisch"
bis der Hauptschlag gestochen wird.
Dabei müssen die anderen Spieler nach Möglichkeit einen Trumpf zugeben oder (mit einem Kritischen) stechen.
Schläge sind von der Regel nicht betroffen. Kann ein Spieler weder noch, dann darf er eine beliebige Karte zugeben.

### Punkte und Ausschaffen

Zu Beginn ist jede Runde 2 Punkte wert. Wenn ein Spieler an der Reihe ist, kann er vor dem Ausspielen oder Zugeben
seiner Karte ausschaffen (mit den Worten "Gehst d?u" oder "Geht ihr?").
Wurde von der Gegenpartei noch keine Karte gelegt, so muss der erste Gegener nach dem Spieler entscheiden, ansonsten
der Gegner, der zuletzt eine Karte gelegt hat.
Der Gegner muss zwischen aufgeben oder weiterspielen wählen. Bei ersterem gewinnt die Partei des Ausschaffenden sofort.
Ansonsten wird weitergespielt, jedoch steigt der Spielwert um 1. Die Parteien können nur abwechselnd ausschaffen.

Hat ein Spieler 9, 13 bzw. 19 Punkte, so ist er gespannt und darf nicht mehr ausschaffen. Tut er es dennoch, so verliert er
oder bei festen Parteien seine Partei 2 Punkte.
Ist ein Spieler gespannt, so ist jede Runde bereits zu Beginn 3 Punkte wert.