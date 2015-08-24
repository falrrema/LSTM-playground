--
----  Copyright (c) 2014, Facebook, Inc.
----  All rights reserved.
----
----  This source code is licensed under the Apache 2 license found in the
----  LICENSE file in the root directory of this source tree. 
<<<<<<< HEAD
=======
<<<<<<< HEAD
----
require('cunn')
LookupTable = nn.LookupTable
=======
>>>>>>> word_mask_LF2

local ok,cunn = pcall(require, 'fbcunn')
if not ok then
    ok,cunn = pcall(require,'cunn')
    if ok then
        print("warning: fbcunn not found. Falling back to cunn") 
        useCUDA = true
        LookupTable = nn.LookupTable
    else
        print("Could not find cunn or fbcunn. Proceeding without CUDA")
        require 'nn'
        useCUDA = false
        LookupTable = nn.LookupTable
    end
else
    deviceParams = cutorch.getDeviceProperties(1)
    cudaComputeCapability = deviceParams.major + deviceParams.minor/10
    LookupTable = nn.LookupTable
    useCUDA = true
end

<<<<<<< HEAD
=======
>>>>>>> master
>>>>>>> word_mask_LF2
require('nngraph')
require('base')
ptb = require('data')
require "layers/MaskedLoss.lua"

-- Train 1 day and gives 82 perplexity.
--[[
local params = {batch_size=20,
                seq_length=35,
                layers=2,
                decay=1.15,
                rnn_size=1500,
                dropout=0.65,
                init_weight=0.04,
                lr=1,
                vocab_size=10000,
                max_epoch=14,
                max_max_epoch=55,
                max_grad_norm=10}
               ]]--

-- Trains 1h and gives test 115 perplexity.
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

<<<<<<< HEAD
=======
<<<<<<< HEAD
function transfer_data(x)
  return x:cuda()
=======
>>>>>>> word_mask_LF2
local function transfer_data(x)
  if useCUDA then
    return x:cuda()
  else
    return x
  end
<<<<<<< HEAD
=======
>>>>>>> master
>>>>>>> word_mask_LF2
end

model = {}

state_train = {}
state_valid = {} 
state_test = {}
paramx = {}
paramdx = {}

function lstm(x, prev_c, prev_h)
  -- Calculate all four gates in one go
  local i2h = nn.Linear(params.rnn_size, 4*params.rnn_size)(x)
  local h2h = nn.Linear(params.rnn_size, 4*params.rnn_size)(prev_h)
  local gates = nn.CAddTable()({i2h, h2h})
  -- Reshape to (batch_size, n_gates, hid_size)
  -- Then slize the n_gates dimension, i.e dimension 2
  local reshaped_gates =  nn.Reshape(4,params.rnn_size)(gates)
  local sliced_gates = nn.SplitTable(2)(reshaped_gates)
  -- Use select gate to fetch each gate and apply nonlinearity
  local in_gate          = nn.Sigmoid()(nn.SelectTable(1)(sliced_gates))
  local in_transform     = nn.Tanh()(nn.SelectTable(2)(sliced_gates))
  local forget_gate      = nn.Sigmoid()(nn.SelectTable(3)(sliced_gates))
  local out_gate         = nn.Sigmoid()(nn.SelectTable(4)(sliced_gates))
  local next_c           = nn.CAddTable()({nn.CMulTable()({forget_gate, prev_c}),nn.CMulTable()({in_gate,     in_transform})})
  local next_h           = nn.CMulTable()({out_gate, nn.Tanh()(next_c)})
  return next_c, next_h
end

function create_network()
  local x                = nn.Identity()()
  local y                = nn.Identity()()
  local prev_s           = nn.Identity()()
  local i                = {[0] = LookupTable(params.vocab_size,
                                                    params.rnn_size)(x)}
  local next_s           = {}
  local split         = {prev_s:split(2 * params.layers)}
  for layer_idx = 1, params.layers do
    local prev_c         = split[2 * layer_idx - 1]
    local prev_h         = split[2 * layer_idx]
    local dropped        = nn.Dropout(params.dropout)(i[layer_idx - 1])
    local next_c, next_h = lstm(dropped, prev_c, prev_h)
    table.insert(next_s, next_c)
    table.insert(next_s, next_h)
    i[layer_idx] = next_h
  end
  local h2y              = nn.Linear(params.rnn_size, params.vocab_size)
  local dropped          = nn.Dropout(params.dropout)(i[params.layers])
  local pred             = nn.LogSoftMax()(h2y(dropped))
  local err              = MaskedLoss()({pred, y})
  local module           = nn.gModule({x, y, prev_s},
                                      {err, nn.Identity()(next_s)})
  module:getParameters():uniform(-params.init_weight, params.init_weight)
  return transfer_data(module)
end

function setup()
  print("Creating a RNN LSTM network.")
  local core_network = create_network()
  paramx, paramdx = core_network:getParameters()
  model.s = {}
  model.ds = {}
  model.start_s = {}
  -- set up table of tables to hold parameters for each time step. Each step in
  -- the sequence has an 2*Nlayers x 1 table holding the parameters.
  for j = 0, params.seq_length do
    model.s[j] = {}
    for d = 1, 2 * params.layers do -- initialise with zeros
      model.s[j][d] = transfer_data(torch.zeros(params.batch_size, params.rnn_size))
    end
  end
  -- Do the same for the gradients but this time do not need to hold each time 
  -- step (presumably the gradient is accumulated for backprop)
  for d = 1, 2 * params.layers do
    model.start_s[d] = transfer_data(torch.zeros(params.batch_size, params.rnn_size))
    model.ds[d] = transfer_data(torch.zeros(params.batch_size, params.rnn_size))
  end
  model.core_network = core_network
  -- Unroll the network through time:
  model.rnns = g_cloneManyTimes(core_network, params.seq_length)
  model.norm_dw = 0
  model.err = transfer_data(torch.zeros(params.seq_length))
end

function reset_state(state)
  state.pos = 1
  if model ~= nil and model.start_s ~= nil then
    for d = 1, 2 * params.layers do
      model.start_s[d]:zero()
    end
  end
end

function reset_ds()
  for d = 1, #model.ds do
    model.ds[d]:zero()
  end
end

function fp(state)
  g_replace_table(model.s[0], model.start_s)
  if state.pos + params.seq_length > state.data:size(1) then
    reset_state(state)
  end
  local y = torch.zeros(params.seq_length)
  for i = 1, params.seq_length do
    local x = state.data[state.pos]
    if i == params.seq_length then
      y = state.data[state.pos + 1]
    end
    local s = model.s[i - 1]
    acc, model.s[i] = unpack(model.rnns[i]:forward({x, y, s}))
    state.pos = state.pos + 1
  end
  g_replace_table(model.start_s, model.s[params.seq_length])
  return acc
end

function bp(state)
  paramdx:zero()
  reset_ds()
  local y = torch.zeros(params.seq_length)
  for i = params.seq_length, 1, -1 do
    state.pos = state.pos - 1
    local x = state.data[state.pos]
    if i == params.seq_length then
      y = state.data[state.pos + 1]
    end
    local s = model.s[i - 1]
    local derr = transfer_data(torch.ones(1))
    local tmp = model.rnns[i]:backward({x, y, s},{derr, model.ds})[3]
    g_replace_table(model.ds, tmp)
    if useCUDA then
      cutorch.synchronize()
    end
  end
  state.pos = state.pos + params.seq_length
  model.norm_dw = paramdx:norm()
  if model.norm_dw > params.max_grad_norm then
    local shrink_factor = params.max_grad_norm / model.norm_dw
    paramdx:mul(shrink_factor)
  end
  paramx:add(paramdx:mul(-params.lr))
end

function run_valid()
  reset_state(state_valid)
  g_disable_dropout(model.rnns)
  local len = (state_valid.data:size(1) - 1) / (params.seq_length)
  local acc = torch.zeros(len)
  for i = 1, len do
    acc[i] = fp(state_valid)
  end
  print("Validation set accuracy : " .. g_f3(acc:mean()))
  g_enable_dropout(model.rnns)
end

function run_test()
  reset_state(state_test)
  g_disable_dropout(model.rnns)
  local len = (state_test.data:size(1) - 1) / (params.seq_length)
  local acc = torch.zeros(len)
  for i = 1, len do
    acc[i] = fp(state_test)
  end
  print("Testing set accuracy : " .. g_f3(acc:mean()))
  g_enable_dropout(model.rnns)
end

function run_sample()
  reset_state(state_test)
  g_disable_dropout(model.rnns)
  local len = (state_test.data:size(1) - 1) / (params.seq_length)
  local acc = torch.zeros(len)
  for i = 1, len do
    acc[i] = fp(state_test)
  end
  print("Testing set accuracy : " .. g_f3(acc:mean()))
  g_enable_dropout(model.rnns)
end



local function main()
  if useCUDA then
    g_init_gpu(arg)
  end
  state_train = {data=transfer_data(ptb.traindataset(params.batch_size))}
  state_valid =  {data=transfer_data(ptb.validdataset(params.batch_size))}
  state_test =  {data=transfer_data(ptb.testdataset(params.batch_size))}
  print("Network parameters:")
  print(params)
  states = {state_train, state_valid, state_test}
  for _, state in pairs(states) do
    reset_state(state)
  end
  setup()
  step = 0
  epoch = 0
  total_cases = 0
  beginning_time = torch.tic()
  start_time = torch.tic()
  print("Starting training.")
  words_per_step = params.seq_length * params.batch_size
  epoch_size = torch.floor(state_train.data:size(1) / params.seq_length)
  acc_sum = 0
  while epoch < params.max_max_epoch do
    local acc = fp(state_train)
    acc_sum = acc_sum + acc
    step = step + 1
    bp(state_train)
    total_cases = total_cases + params.seq_length * params.batch_size
    epoch = step / epoch_size
    --print('train accuracy = ' .. g_f3(acc_sum / step))
    if step % torch.round(epoch_size / 20) == 20 then
      local wps = torch.floor(total_cases / torch.toc(start_time))
      local since_beginning = g_d(torch.toc(beginning_time) / 60)
      print('epoch = ' .. g_f3(epoch) ..
            ', train accuracy = ' .. g_f3(acc_sum / step) ..
            ', wps = ' .. wps ..
            ', dw:norm() = ' .. g_f3(model.norm_dw) ..
            ', lr = ' ..  g_f3(params.lr) ..
            ', since beginning = ' .. since_beginning .. ' mins.')
    end
    if step % epoch_size == 0 then
      run_valid()
      if epoch > params.max_epoch then
          params.lr = params.lr / params.decay
      end
    end
<<<<<<< HEAD
=======
<<<<<<< HEAD
    if step % 33 == 0 then -- Is this related to the number of CUDA cores?
      cutorch.synchronize()
=======
>>>>>>> word_mask_LF2
    if step % 33 == 0 then
      if useCUDA then
        cutorch.synchronize()
      end
<<<<<<< HEAD
=======
>>>>>>> master
>>>>>>> word_mask_LF2
      collectgarbage()
    end
  end
  run_test()
  print("Training is over.")
end

main()