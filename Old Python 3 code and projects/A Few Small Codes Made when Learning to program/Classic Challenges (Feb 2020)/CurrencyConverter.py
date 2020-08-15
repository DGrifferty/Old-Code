currency = {
    'GDP' : 1.3,
    'EUR' : 1.08,
    'USD' : 1.0,
    'AUD' : 0.66,
    'JPY' : 0.0090
}

while True:
    intialcur = str(input('Please Enter the currency you want to convert from\n: ')).upper()
    while True:
        if intialcur in currency:
            break
        else:
            intialcur = str(input('Not in list. Please Enter the currency you want to convert from\n: ')).upper()

    intialvalue = input('Please enter the amount of that currency\n: ')

    while True:
        try:
            float(intialvalue)
            break
        except ValueError:
            intialvalue = input('Not a number. Please enter the amount of that currency\n: ')

    finalcur = str(input('Enter the currency you want to convert this to\n: ')).upper()

    while True:
        if finalcur in currency:
            break
        else:
            finalcur = str(input('Not in list. Please Enter the currency you want to convert from\n: ')).upper()

    finalvalue = (float(currency.get(str(intialcur))) * float(intialvalue)) / float(currency.get(str(finalcur)))

    print(finalvalue)

    decision = input('Would you like to convert more values?').lower()

    if decision == 'no':
        print('Goodbye')
        exit()
    elif decision != 'no' and decision != 'yes':
        decision = input('Would you like to convert more values? Please enter yes or no\n: ').lower()