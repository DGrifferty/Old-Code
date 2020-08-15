import TicTacModulePyGame
from TicTacModulePyGame import *

DisplayLogo()

while True:

    if TicTacModulePyGame.UserCirclesOrCross():  # returns true if human chooses circle
        HumanMark, ComputerMark = Circle, Cross
    else:
        HumanMark, ComputerMark = Cross, Circle

    TicTacModulePyGame.UserFirstOrSecond(Board, ComputerMark, Circle, Cross, HumanMark)

    while True:

        Board[int(TicTacModulePyGame.UserPlace(Board, Empty, TopLeftData))] = HumanMark  # Module asks user where they want to place their marker

        if TicTacModulePyGame.Checkwin(Board, HumanMark, TopLeftData):  # Checks win, returns true if someone has won
            pygame.time.delay(800)
            if PlayAgain():
                Board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                continue

        if TicTacModulePyGame.DrawCheck(Board, TopLeftData):  # Returns true if board is full.
            pygame.time.delay(800)
            if PlayAgain():
                Board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                continue

        TicTacModulePyGame.ComputerPlace(Board, ComputerMark, Circle, Cross, HumanMark)  # computer places mark

        if TicTacModulePyGame.Checkwin(Board, HumanMark, TopLeftData):  # Returns True is someone has won
            pygame.time.delay(800)
            if PlayAgain():
                Board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                continue

        if TicTacModulePyGame.DrawCheck(Board, TopLeftData):  # Returns true if board is full.
            pygame.time.delay(800)
            if PlayAgain():
                Board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                continue

        TicTacModulePyGame.BoardPrint(Board)

    # TODO: Music
    # TODO: Store wins and losses, sounds, difficulty levels
    # TODO: Sort out corners
