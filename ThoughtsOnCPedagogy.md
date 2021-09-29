# Thoughts on C's pedagogical legacy

C needs to be viewed through the lens that it began on the PDP-11 and has warts from that era.

If you treat C as a language designed explicitly to bootstrap Unix's development then its design becomes more apparent and tolerable.

Another common pedagogical pitfall is the notion of the C abstract machine not being taught. I can't count how many times teachers at my Uni would belabour concept of the stack and the heap without taking the C abstract machine into context. It wasn't until my Computer architecture class that out of order execution was talked about either. Not that these details are critical for most computing you'll be expressing. Ideally the libraries and database applications you'll be using are anal about those things.

More importantly however theres a false sense of confidence people have, when using higher level languages like Python/JavaScript, that C is this low level bedrock when so much more is going on behind the scene. In turn, knowing these things let you criticise C, and its derivatives for its effects on modern programming.
