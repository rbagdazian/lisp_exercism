Class {
	#name : #TwoFer,
	#superclass : #Object,
	#category : #'Exercise@Two-Fer'
}
{ #category : #example }
TwoFer  >> who  [
	^ 'One for you, one for me.'.
]
{ #category : #example }
TwoFer >> who: name [
	^ 'One for ',name, ', one for me.'.
]