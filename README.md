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
Usually you play until one player's score reaches 15 (or 21).
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
In the first trick, if the first card is the "Hauptschlag", then any player MUST play a trump card or a crit.
If both is not possible, the player may play any card.
Once a crit is played by a opposing player, the exception is void.

### Score

At the beginning very round is worth 2 points.
In your turn, you can "ausschaffen".
Then the opposing players have to decide if they want to give up.
If they don't give up, the round is continued and worth one more point.

At the beginning of each round any team may do the first "Ausschaffen", but then it has to alternate.
If you used "ausschaffen" and your opponents choose not to do, you can't increse the round's value.

When a player has a score of 13 (or 19) he/she is called "gespannt" and may not "ausschaffen". If they do, they lose 2 points of their score.
In a game with fixed teams, when a team is "gespannt", every round is started with an "Ausschaffen" by the lower team, if they are not "gespannt" either.