-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
----- 				Experimenting and undestanding Word-level LSTM				-----
-------------------------------------------------------------------------------------

-- Data loading --

stringx = require('pl.stringx')
file = require('pl.file')

ptb_path = "./data/"

vocab_idx = 0
vocab_map = {}

train = file.read(ptb_path .. "ptb.train.txt") -- We'll do it for train
train = stringx.replace(train, '\n', '<eos>') -- Replace line breaks with <eos>
train = stringx.split(train) -- Splits data into table, one word per entry

print(string.format("Loading %s, size of data = %d", "./data/ptb.train.txt", #train))

-- Creates a map of the vocabulary of the whole document in vocab_map assigning a
-- unique number called vocab_idx. Then creates x that is a tensor that transformed the words
-- into the correct unique vocab_idx
x = torch.zeros(#train)
for i = 1, #train do
    if vocab_map[train[i]] == nil then
    vocab_idx = vocab_idx + 1
    vocab_map[train[i]] = vocab_idx 
	end
x[i] = vocab_map[train[i]]
end

-- This is the expanded replicate function, the idea is to feed the training data by 
--batches then for example if batches = 20, since data set has 929589 words, the it will
-- batches of 929589/20 = 46479 words. Thus, it creates "w" which is 46479 x 20 and 
-- in each column a batch of 46479 is aggregated filling the tensor with the for loop. 
s = x:size(1)
w = torch.zeros(torch.floor(s / batch_size), batch_size)
for i = 1, batch_size do
     start = torch.round((i - 1) * s / batch_size) + 1
     finish = start + w:size(1) - 1
     w:sub(1, w:size(1), i, i):copy(x:sub(start, finish))
end

