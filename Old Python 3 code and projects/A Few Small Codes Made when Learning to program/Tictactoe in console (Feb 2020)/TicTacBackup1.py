# Tictactoe game
import random

Circle, Cross, Empty, Winner, NumberOfGoes = 'O', 'X', '.', False, 0
CirclePlaces, CrossPlaces = [], []
WinningCombinations = ([0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6])
# replace empty with numbers
Board = [Empty, Empty, Empty,
         Empty, Empty, Empty,
         Empty, Empty, Empty]

CircleOrCross = str(input('Would you like to be circles or crosses?\n: ')).lower()

while True:
    if CircleOrCross == 'circle' or CircleOrCross == 'o' or CircleOrCross == 'circles':
        HumanMark = Circle
        ComputerMark = Cross
        break
    elif CircleOrCross == 'cross' or CircleOrCross == 'x' or CircleOrCross == 'crosses':
        HumanMark = Cross
        ComputerMark = Circle
        break
    else:
        CircleOrCross = str(input('Would you like to be circles or crosses? Please enter circle or cross. \n: ')).lower()


def Checkwin(Board, HumanMark, ComputerMark):
    global Winner
    i = -1
    for element in Board:
        i += 1
        if element == Circle:
            CirclePlaces.append(i)
        elif element == Cross:
            CrossPlaces.append(i)
    for combination in WinningCombinations:
        if combination[0] in CirclePlaces and combination[1] in CirclePlaces and combination[2] in CirclePlaces:
            if HumanMark == Circle:
                print('You win!')
                Winner = True
            else:
                print('Computer wins!')
                Winner = True
        elif combination[0] in CrossPlaces and combination[1] in CrossPlaces and combination[2] in CrossPlaces:
            if HumanMark == Cross:
                print('You win!')
                Winner = True
            else:
                print('Computer wins!')
                Winner = True


def ComputerPlace(Board, ComputerMark):
    while True:

        if Board[4] == Empty:
            Board[4] = ComputerMark
            break

        else:
            place = random.randint(0, 8)

            if Board[place] == Empty:
                Board[int(place)] = ComputerMark
                break


while True:

    place = float(input('Enter where you would like to place your marker?\n: '))

    while True:
        if place == round(place) and Board[int(place - 1)] == Empty:  # Check if number and place is empty
            break
        else:
            place = float(input('Enter where you would like to place your marker? Please select an integer between 1 and 9 and select an empty place.\n: '))

    Board[int(place - 1)] = HumanMark

    Checkwin(Board, HumanMark, ComputerMark)

    if Winner == True:
        print(' | '.join(Board[0:3]))
        print(' | '.join(Board[3:6]))
        print(' | '.join(Board[6:9]))
        exit()

    NumberOfGoes += 1

    if NumberOfGoes == 9:
        print(' | '.join(Board[0:3]))
        print(' | '.join(Board[3:6]))
        print(' | '.join(Board[6:9]))
        print('Board full, Draw!')
        exit()

    ComputerPlace(Board, ComputerMark)

    Checkwin(Board, HumanMark, ComputerMark)

    if Winner == True:
        print(' | '.join(Board[0:3]))
        print(' | '.join(Board[3:6]))
        print(' | '.join(Board[6:9]))
        exit()

    NumberOfGoes += 1

    print(' | '.join(Board[0:3]))
    print(' | '.join(Board[3:6]))
    print(' | '.join(Board[6:9]))

# TODO: Create a function for human place and computer place to allow either user to go first
# TODO: Put everything in module and give computer more advanced ui, make functions return true etc.
# TODO: Create gui, and music
# TODO: Store wins and losses, sounds, difficulty levels
# TODO: print board function