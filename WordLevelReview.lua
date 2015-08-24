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
batch_size = 20
s = x:size(1)
w = torch.zeros(torch.floor(s / batch_size), batch_size)
for i = 1, batch_size do
     start = torch.round((i - 1) * s / batch_size) + 1
     finish = start + w:size(1) - 1
     w:sub(1, w:size(1), i, i):copy(x:sub(start, finish))
end

-- Now the same for test --

vocab_idx2 = 0
vocab_map2 = {}

test = file.read(ptb_path .. "ptb.test.txt") -- We'll do it for train
test = stringx.replace(test, '\n', '<eos>') -- Replace line breaks with <eos>
test = stringx.split(test) -- Splits data into table, one word per entry

print(string.format("Loading %s, size of data = %d", "./data/ptb.test.txt", #test))

-- Creates a map of the vocabulary of the whole document in vocab_map assigning a
-- unique number called vocab_idx. Then creates x that is a tensor that transformed the words
-- into the correct unique vocab_idx
x2 = torch.zeros(#validation)
for i = 1, #validation do
    if vocab_map2[validation[i]] == nil then
    vocab_idx2 = vocab_idx2 + 1
    vocab_map2[validation[i]] = vocab_idx2 
	end
x2[i] = vocab_map2[validation[i]]
end

x2 = x2:resize(x2:size(1), 1):expand(x2:size(1), batch_size) -- makes 20 batches of the same text data

--Now for the Validation -- 

vocab_idx3 = 0
vocab_map3 = {}

validation = file.read(ptb_path .. "ptb.valid.txt") -- We'll do it for train
validation = stringx.replace(validation, '\n', '<eos>') -- Replace line breaks with <eos>
validation = stringx.split(validation) -- Splits data into table, one word per entry

print(string.format("Loading %s, size of data = %d", "./data/ptb.validation.txt", #validation))

-- Creates a map of the vocabulary of the whole document in vocab_map assigning a
-- unique number called vocab_idx. Then creates x that is a tensor that transformed the words
-- into the correct unique vocab_idx
x3 = torch.zeros(#validation)
for i = 1, #validation do
    if vocab_map3[validation[i]] == nil then
    vocab_idx3 = vocab_idx3+ 1
    vocab_map3[validation[i]] = vocab_idx3
	end
x3[i] = vocab_map3[validation[i]]
end

batch_size = 20
s2 = x3:size(1)
w2 = torch.zeros(torch.floor(s2 / batch_size), batch_size)
for i = 1, batch_size do
     start = torch.round((i - 1) * s2 / batch_size) + 1
     finish = start + w2:size(1) - 1
     w2:sub(1, w2:size(1), i, i):copy(x3:sub(start, finish)) 
end

--- Model Main ---

--Loads the base.lua and assigns the data.lua to ptb.
require('nngraph')
require('base')

ptb = require('data')

LookupTable = nn.LookupTable -- use for word embeddings

--Sets parameters that this model will have. 
params = {batch_size=20,
                seq_length=20,
                layers=2,
                decay=2,
                rnn_size=200,
                dropout=0,
                init_weight=0.1,
                lr=1,
                vocab_size=10000,
                max_epoch=4,
                max_max_epoch=13,
                max_grad_norm=5}
-- Network --

x                = nn.Identity()()
y                = nn.Identity()()
prev_s           = nn.Identity()()
i                = {[0] = LookupTable(params.vocab_size,
                                                    params.rnn_size)(x)}
next_s           = {}
split         = {prev_s:split(2 * params.layers)}
  for layer_idx = 1, params.layers do
    prev_c         = split[2 * layer_idx - 1]
    prev_h         = split[2 * layer_idx]
    dropped        = nn.Dropout(params.dropout)(i[layer_idx - 1])
    next_c, next_h = lstm(dropped, prev_c, prev_h)
    table.insert(next_s, next_c)
    table.insert(next_s, next_h)
    i[layer_idx] = next_h
  end
  h2y              = nn.Linear(params.rnn_size, params.vocab_size)
  dropped          = nn.Dropout(params.dropout)(i[params.layers])
  pred             = nn.LogSoftMax()(h2y(dropped))
  err              = nn.ClassNLLCriterion()({pred, y})
  module           = nn.gModule({x, y, prev_s},
                                      {err, nn.Identity()(next_s)})
  module:getParameters():uniform(-params.init_weight, params.init_weight)
  return transfer_data(module)
end


--LSTM--
-- Calculate all four gates in one go

i2h = nn.Linear(params.rnn_size, 4*params.rnn_size)(x)
h2h = nn.Linear(params.rnn_size, 4*params.rnn_size)(prev_h)
gates = nn.CAddTable()({i2h, h2h})
  
  -- Reshape to (batch_size, n_gates, hid_size)
  -- Then slize the n_gates dimension, i.e dimension 2
  local reshaped_gates =  nn.Reshape(4,params.rnn_size)(gates)
  local sliced_gates = nn.SplitTable(2)(reshaped_gates)
  
  -- Use select gate to fetch each gate and apply nonlinearity
  local in_gate          = nn.Sigmoid()(nn.SelectTable(1)(sliced_gates))
  local in_transform     = nn.Tanh()(nn.SelectTable(2)(sliced_gates))
  local forget_gate      = nn.Sigmoid()(nn.SelectTable(3)(sliced_gates))
  local out_gate         = nn.Sigmoid()(nn.SelectTable(4)(sliced_gates))

  local next_c           = nn.CAddTable()({
      nn.CMulTable()({forget_gate, prev_c}),
      nn.CMulTable()({in_gate,     in_transform})
  })
  local next_h           = nn.CMulTable()({out_gate, nn.Tanh()(next_c)})

  return next_c, next_h
end












































