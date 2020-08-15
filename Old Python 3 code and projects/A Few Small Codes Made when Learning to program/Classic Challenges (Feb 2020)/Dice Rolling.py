import random

while True:
    try:
        dice  = int(input('Please enter the size of the dice you would like to roll\n: '))
        if dice == round(dice):
            break
        elif dice != round(dice):
            dice = int(input('Please enter an integer number\n: '))
    except Exception as err:
        print(err)
        dice = input('Please enter an integer!\n: ')

while True:

    roll = random.randint(0,dice)
    print('Dice rolled ' + str(roll))

    decision = input('Would you like to roll again? \n: ')
    if decision.upper() == 'YES':
        print('Rolling again!')
    elif decision.upper() == 'NO':
        print('Goodbye!')
        exit()
    else:
        while True:
            decision = input('Please enter yes or no\n: ')

            if decision.upper() == 'YES':
                print('Rolling again!')
                break
            elif decision.upper() == 'NO':
                print('Goodbye!')
                exit()