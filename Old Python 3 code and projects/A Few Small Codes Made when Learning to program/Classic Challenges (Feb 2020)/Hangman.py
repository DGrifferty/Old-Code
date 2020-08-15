secret_word = 'fishman'
guessed_letters = []
no_of_trys = 0
while True:
	while True:
		try:
			guess = input('Enter your guess of a letter ')
			guess = guess.lower()
			if guess.lower() not in 'abcdefghijklmnopqrstuvwxyz':
				raise Exception('Please enter a letter')
			elif len(guess) == 1 and guess not in guessed_letters:
				guessed_letters.append(guess)
				no_of_trys +=1
				break
			elif guess in guessed_letters:
				print('Please enter a new letter')
				continue
			elif len(guess) != 1:
				raise Exception('Please enter a single letter')

		except Exception as err:
			print(err)

	win = True

	for letter in secret_word:
		if letter in guessed_letters:
			print(letter, end='')
		else:
			print('_', end='')
			win = False

	print('\n')

	if win == True:
		print(f'You win! the word was {secret_word}')
		print(f'It took you {no_of_trys} guessed to guess a {len(secret_word)} letter word')
		print(f'That is {len(secret_word)/no_of_trys} guesses per word')
		break
