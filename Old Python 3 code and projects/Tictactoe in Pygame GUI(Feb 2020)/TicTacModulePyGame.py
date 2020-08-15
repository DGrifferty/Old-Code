import random
import pygame

Circle, Cross, Empty, Winner, NumberOfGoes = 'O', 'X', '.', False, 0
WinningCombinations = ([0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6])
Board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
TopLeftData = [0, 0, 0]
pygame.init()
clock = pygame.time.Clock()
pygame.mouse.set_cursor(*pygame.cursors.broken_x)
BackgroundColour = (70, 180, 255)
TextColour = (0, 0, 0)
SoundSettings = open('SoundSettings.txt', 'r+')
SoundOn = SoundSettings.readline()
display_width = 800
display_height = 800
size = width, height = display_width, display_height
surface = pygame.display.set_mode(size)
pygame.display.set_caption("TicTacToe by Dom")
surface.fill(BackgroundColour)
EmptyBoard = pygame.image.load('emptyboard.png')
X_Image = pygame.image.load('X.png')
O_Image = pygame.image.load('O.png')
One_Image = pygame.image.load('1.png')
Two_Image = pygame.image.load('2.png')
X_Board_Image = pygame.image.load('XBoard.png')
O_Board_Image = pygame.image.load('OBoard.png')
Yes_Image = pygame.image.load('Yes.png')
No_Image = pygame.image.load('No.png')
Logo = pygame.image.load('logo.jpg').convert()
TextFont = pygame.font.Font('Spartan-Regular.ttf', 20)

TopTextPosition = (display_width * 0.2, display_height * 0.25)
LeftOptionX = display_width * 0.2
LeftOptionY = display_height * 0.45
RightOptionX = display_width * 0.5
RightOptionY = display_height * 0.45
Boardx = display_width * 0.23
Boardy = display_height * 0.35
TopLeftDatax = display_width * 0.1
TopLeftDatay = display_height * 0.1
LogoPosition = (display_width * 0.05, display_height * 0.2)


def DisplayLogo():
    pygame.mixer.music.load('Quack.mp3')
    pygame.mixer.music.play()
    pygame.mixer.music.play()

    for i in range(225):
        surface.fill((0, 0, 0))
        Logo.set_alpha(i)
        surface.blit(Logo, LogoPosition)
        pygame.display.flip()
        pygame.time.delay(10)

    pygame.mixer.music.load('pop.mp3')


def ReadSound():

    SoundSettings = open('SoundSettings.txt', 'r+')
    SoundOn = SoundSettings.readlines()
    SoundOn = ''.join(SoundOn)
    if 'One' in SoundOn:
        return True
    elif 'Two' in SoundOn:
        return False


def ChangeSound():
    SoundSettings = open('SoundSettings.txt', 'r+')
    print(SoundOn)
    if ReadSound():
        SoundSettings.truncate()
        SoundSettings.write('Two')
        print('Here')

    elif not ReadSound():
        SoundSettings.truncate()
        SoundSettings.write('One')


def DisplaySound():
    SoundOn = ReadSound()
    print(SoundOn)

    rect = pygame.Rect((0, 0, 110, 20))
    surface.fill(BackgroundColour, rect)

    if SoundOn:
        text = TextFont.render('Sound on', True, TextColour)
        surface.blit(text, (0, 0))
        pygame.display.update(rect)
        return True

    elif not SoundOn:
        text = TextFont.render('Sound off', True, TextColour)
        surface.blit(text, (0, 0))
        pygame.display.update(rect)
        return False




def BoardPrint(Board):
    surface.blit(EmptyBoard, (Boardx, Boardy))

    positionlist = [[Boardx, Boardy], [Boardx + 130, Boardy], [Boardx + 260, Boardy],
                    [Boardx, Boardy + 130], [Boardx + 130, Boardy + 130], [Boardx + 260, Boardy + 130],
                    [Boardx, Boardy + 260], [Boardx + 130, Boardy + 260], [Boardx + 260, Boardy + 260]]
    i = -1
    for element in Board:
        i += 1
        position = positionlist[i]
        if element == Circle:
            surface.blit(O_Board_Image, (position[0], position[1]))
        elif element == Cross:
            surface.blit(X_Board_Image, (position[0], position[1]))
    pygame.display.update()


def UserCirclesOrCross():
    surface.fill(BackgroundColour)
    surface.blit(X_Image, (LeftOptionX, LeftOptionY))
    surface.blit(O_Image, (RightOptionX, RightOptionY))
    text = TextFont.render('Would you like to be circles or crosses?', True, TextColour)
    surface.blit(text, TopTextPosition)
    DisplaySound()
    pygame.display.update()


    while True:

        x, y = pygame.mouse.get_pos()

        if x <= LeftOptionX + 40 and x > LeftOptionX and y <= LeftOptionY + 40 and y > LeftOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)

        elif RightOptionX + 40 >= x > RightOptionX and RightOptionY + 40 >= y > RightOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_right)
        elif 110 >= x > 0 and 20 >= y > 0:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)
        else:
            pygame.mouse.set_cursor(*pygame.cursors.broken_x)

        for event in pygame.event.get():

            if event.type == pygame.QUIT:
                pygame.quit()
                quit()

            if event.type == pygame.MOUSEBUTTONDOWN:
                x, y = event.pos
                if 110 >= x > 0 and 20 >= y > 0:
                    ChangeSound()
                    DisplaySound()
                if x <= LeftOptionX + 40 and x > LeftOptionX and y <= LeftOptionY + 40 and y > LeftOptionY:
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return False
                if x <= RightOptionX + 40 and x > RightOptionX and y <= RightOptionY + 40 and y > RightOptionY:
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return False


def UserFirstOrSecond(Board, ComputerMark, Circle, Cross, HumanMark):
    surface.fill(BackgroundColour)
    surface.blit(One_Image, (LeftOptionX, LeftOptionY))
    surface.blit(Two_Image, (RightOptionX, RightOptionY))

    text = TextFont.render('Would you like to go first or second?', True, TextColour)
    surface.blit(text, TopTextPosition)
    DisplaySound()
    pygame.display.update()

    while True:

        x, y = pygame.mouse.get_pos()

        if x <= LeftOptionX + 40 and x > LeftOptionX and y <= LeftOptionY + 40 and y > LeftOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)

        elif x <= RightOptionX + 40 and x > RightOptionX and y <= RightOptionY + 40 and y > RightOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_right)

        elif 110 >= x > 0 and 20 >= y > 0:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)

        else:
            pygame.mouse.set_cursor(*pygame.cursors.broken_x)

        for event in pygame.event.get():

            if event.type == pygame.MOUSEBUTTONDOWN:
                x, y = event.pos
                if 110 >= x > 0 and 20 >= y > 0:
                    ChangeSound()
                    DisplaySound()
                if x <= LeftOptionX + 40 and x > LeftOptionX and y <= LeftOptionY + 40 and y > LeftOptionY:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return False
                if x <= RightOptionX + 40 and x > RightOptionX and y <= RightOptionY + 40 and y > RightOptionY:
                    ComputerPlace(Board, ComputerMark, Circle, Cross, HumanMark)
                    BoardPrint(Board)
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return True
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()


def Checkwin(Board, HumanMark, TopLeftData):
    i = -1
    CirclePlaces, CrossPlaces = [], []
    for element in Board:
        i += 1
        if element == Circle:
            CirclePlaces.append(i)
        elif element == Cross:
            CrossPlaces.append(i)
    for combination in WinningCombinations:
        if combination[0] in CirclePlaces and combination[1] in CirclePlaces and combination[2] in CirclePlaces:
            if HumanMark == Circle:
                surface.fill(BackgroundColour)
                text = TextFont.render('You Win! ', True, TextColour)
                surface.blit(text, TopTextPosition)
                BoardPrint(Board)
                pygame.display.update()
                TopLeftData[0] += 1
                return True
            else:
                surface.fill(BackgroundColour)
                text = TextFont.render('Computer wins! ', True, TextColour)
                surface.blit(text, TopTextPosition)
                BoardPrint(Board)
                pygame.display.update()
                TopLeftData[2] += 1
                return True
        elif combination[0] in CrossPlaces and combination[1] in CrossPlaces and combination[2] in CrossPlaces:
            if HumanMark == Cross:
                surface.fill(BackgroundColour)
                text = TextFont.render('You Win! ', True, TextColour)
                surface.blit(text, TopTextPosition)
                BoardPrint(Board)
                pygame.display.update()
                TopLeftData[0] += 1
                return True
            else:
                surface.fill(BackgroundColour)
                text = TextFont.render('Computer wins! ', True, TextColour)
                surface.blit(text, TopTextPosition)
                BoardPrint(Board)
                pygame.display.update()
                TopLeftData[2] += 1
                return True


def ComputerPlace(Board, ComputerMark, Circle, Cross, HumanMark):
    while True:

        if Board[4] == Empty:
            Board[4] = ComputerMark
            break

        elif ComputerAboutToWin(Board, ComputerMark, Circle, Cross):
            Board[int(BlankPlace)] = ComputerMark
            break

        elif Humanabouttowin(Board, HumanMark, Circle, Cross):
            Board[int(BlankPlace)] = ComputerMark
            break

        else:
            place = random.randint(0, 8)
            if Board[place] == Empty:
                Board[int(place)] = ComputerMark
                break


def Humanabouttowin(Board, HumanMark, Circle, Cross):
    global BlankPlace
    CirclePlaces, CrossPlaces = [], []
    i = -1
    for element in Board:
        i += 1
        if element == Circle:
            CirclePlaces.append(i)
        elif element == Cross:
            CrossPlaces.append(i)

    if HumanMark == Circle:
        for combination in WinningCombinations:
            lst = []
            for p in range(0, 3):
                if combination[p] in CirclePlaces:
                    lst.append(combination[p])
                elif combination[p] in CrossPlaces:
                    lst = []
                    break
                else:
                    BlankPlace = combination[p]
            if len(lst) == 2:
                return True


    elif HumanMark == Cross:
        for combination in WinningCombinations:
            lst = []
            for p in range(0, 3):
                if combination[p] in CrossPlaces:
                    lst.append(combination[p])
                elif combination[p] in CirclePlaces:
                    lst = []
                    break
                else:
                    BlankPlace = combination[p]
            if len(lst) == 2:
                return True

    else:
        return False


def ComputerAboutToWin(Board, ComputerMark, Circle, Cross):
    global BlankPlace
    CirclePlaces, CrossPlaces = [], []
    i = -1
    for element in Board:
        i += 1
        if element == Circle:
            CirclePlaces.append(i)
        elif element == Cross:
            CrossPlaces.append(i)

    if ComputerMark == Circle:
        for combination in WinningCombinations:
            lst = []
            for p in range(0, 3):
                if combination[p] in CirclePlaces:
                    lst.append(combination[p])
                elif combination[p] in CrossPlaces:
                    lst = []
                    break
                else:
                    BlankPlace = combination[p]
            if len(lst) == 2:
                return True

    elif ComputerMark == Cross:
        for combination in WinningCombinations:
            lst = []
            for p in range(0, 3):
                if combination[p] in CrossPlaces:
                    lst.append(combination[p])
                elif combination[p] in CirclePlaces:
                    lst = []
                    break
                else:
                    BlankPlace = combination[p]
            if len(lst) == 2:
                return True
    else:
        return False


def DrawCheck(Board, TopLeftData):
    if Empty not in Board:
        surface.fill(BackgroundColour)
        text = TextFont.render('Board Full, Draw! ', True, TextColour)
        surface.blit(text, TopTextPosition)
        BoardPrint(Board)
        pygame.display.update()
        TopLeftData[1] += 1
        return True


def UserPlace(Board, Empty, TopLeftData):
    surface.fill(BackgroundColour)
    text = TextFont.render('Play!', True, TextColour)
    surface.blit(text, TopTextPosition)
    text = TextFont.render('W:D:L', True, TextColour)
    surface.blit(text, (TopLeftDatax, TopLeftDatay))
    text = TextFont.render(str(TopLeftData), True, TextColour)
    surface.blit(text, (TopLeftDatax, TopLeftDatay + 0.05 * display_height))
    BoardPrint(Board)
    DisplaySound()
    pygame.display.update()

    while True:

        x, y = pygame.mouse.get_pos()

        if x <= Boardx + 129 and x > Boardx and y <= Boardy + 129 and y > Boardy and Board[0] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 129 and y > Boardy and Board[1] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 129 and y > Boardy and Board[2] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 129 and x > Boardx and y <= Boardy + 259 and y > Boardy + 130 and Board[3] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 259 and y > Boardy + 130 and Board[4] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 259 and y > Boardy + 130 and Board[5] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 129 and x > Boardx and y <= Boardy + 390 and y > Boardy + 260 and Board[6] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 390 and y > Boardy + 260 and Board[7] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 390 and y > Boardy + 260 and Board[8] == Empty:
            pygame.mouse.set_cursor(*pygame.cursors.diamond)
        elif 110 >= x > 0 and 20 >= y > 0:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)
        else:
            pygame.mouse.set_cursor(*pygame.cursors.broken_x)

        for event in pygame.event.get():

            if event.type == pygame.MOUSEBUTTONDOWN:
                x, y = event.pos

                if x <= Boardx + 129 and x > Boardx and y <= Boardy + 129 and y > Boardy and Board[0] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 0
                elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 129 and y > Boardy and Board[1] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 1
                elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 129 and y > Boardy and Board[2] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 2
                elif x <= Boardx + 129 and x > Boardx and y <= Boardy + 259 and y > Boardy + 130 and Board[3] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 3
                elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 259 and y > Boardy + 130 and Board[4] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 4
                elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 259 and y > Boardy + 130 and Board[5] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 5
                elif x <= Boardx + 129 and x > Boardx and y <= Boardy + 390 and y > Boardy + 260 and Board[6] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 6
                elif x <= Boardx + 259 and x > Boardx + 130 and y <= Boardy + 390 and y > Boardy + 260 and Board[7] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 7
                elif x <= Boardx + 390 and x > Boardx + 260 and y <= Boardy + 390 and y > Boardy + 260 and Board[8] == Empty:
                    surface.fill(BackgroundColour)
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    return 8
                elif 110 >= x > 0 and 20 >= y > 0:
                    ChangeSound()
                    DisplaySound()

            if event.type == pygame.QUIT:
                pygame.quit()
                quit()


def PlayAgain():
    surface.fill(BackgroundColour)
    text = TextFont.render('Would you like to play again?', True, TextColour)
    surface.blit(text, TopTextPosition)
    surface.blit(Yes_Image, (LeftOptionX, LeftOptionY))
    surface.blit(No_Image, (RightOptionX, RightOptionY))
    DisplaySound()
    pygame.display.update()

    while True:

        x, y = pygame.mouse.get_pos()

        if x <= LeftOptionX + 120 and x > LeftOptionX and y <= LeftOptionY + 50 and y > LeftOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)

        elif x <= RightOptionX + 120 and x > RightOptionX and y <= RightOptionY + 50 and y > RightOptionY:
            pygame.mouse.set_cursor(*pygame.cursors.tri_right)
        elif 110 >= x > 0 and 20 >= y > 0:
            pygame.mouse.set_cursor(*pygame.cursors.tri_left)

        else:
            pygame.mouse.set_cursor(*pygame.cursors.broken_x)

        for event in pygame.event.get():

            if event.type == pygame.MOUSEBUTTONDOWN:
                x, y = event.pos
                if x <= LeftOptionX + 120 and x > LeftOptionX and y <= LeftOptionY + 50 and y > LeftOptionY:
                    pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    surface.fill(BackgroundColour)
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    BoardPrint(Board)
                    return True
                elif x <= RightOptionX + 120 and x > RightOptionX and y <= RightOptionY + 50 and y > RightOptionY:
                    surface.fill(BackgroundColour)
                    text = TextFont.render('Thanks for playing!', True, TextColour)
                    surface.blit(text, TopTextPosition)
                    pygame.display.update()
                    pygame.mixer.music.load('Quack.mp3')
                    if ReadSound():
                        pygame.mixer.music.play()
                        pygame.mixer.music.play()
                    pygame.time.delay(2000)
                    pygame.quit()
                    quit()
                elif 110 >= x > 0 and 20 >= y > 0:
                    ChangeSound()
                    DisplaySound()
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()
