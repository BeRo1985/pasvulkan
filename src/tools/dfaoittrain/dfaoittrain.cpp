// This tool trains a neural network to approximate the correct OIT color for a given set of colors for usage in a fragment shader.
// The neural network is trained with all possible color combinations for a given colo count. 

// Copyright (C) 2023 by Benjamin 'BeRo' Rosseaux - Licensed under the zlib license

#include <ostream>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cstdint>
#include <vector>
#include <cmath>
#include <random>
#include <unordered_map>
#include <array>
#include "pcg_random.hpp"

//std::random_device randomDevice;
///std::mt19937 randomNumberGenerator(randomDevice());

//pcg_extras::seed_seq_from<std::random_device> seed_source;
//pcg32 randomNumberGenerator(seed_source);

pcg32 randomNumberGenerator(0x853c49e6748fea9bULL, 0xda3e39cb94b95bdbULL); // fixed seed for reproducibility (on the same machine with the same compiler with the same compiler settings)   

static double activationFunction_relu(double x){
  return (x > 0.0) ? x : 0.0;
}

static double activationFunction_relu_derivative(double x){
  return (x > 0.0) ? 1.0 : 0.0;
}

static double activationFunction_sigmoid(double x) {
  return 1.0 / (1.0 + exp(-x));
}

static double activationFunction_sigmoid_derivative(double x) {
  return x * (1.0 - x);
}

typedef double (*ActivationFunctionFunction)(double);
typedef double (*ActivationFunctionDerivative)(double); 

struct ActivationFunction {
  ActivationFunctionFunction m_function;
  ActivationFunctionDerivative m_derivative;
};

static ActivationFunction activationFunctionRELU = { activationFunction_relu, activationFunction_relu_derivative };
static ActivationFunction activationFunctionSigmoid = { activationFunction_sigmoid, activationFunction_sigmoid_derivative };

class Layer {
public:
  std::vector<double> m_outputs;
  std::vector<double> m_gradients;
  std::vector<std::vector<double>> m_weights;
  std::vector<double> m_biases; 
  ActivationFunction m_activationFunction;
  double m_learningRate = 0.00001;

  Layer(ssize_t input_size, ssize_t output_size, const ActivationFunction& activationFunction = activationFunctionSigmoid) : m_activationFunction(activationFunction) {

    // Initialize weights and biases with random values between -1 and 1
    std::uniform_real_distribution<> distribution(-1, 1);

    m_weights.resize(output_size, std::vector<double>(input_size));
    for(ssize_t i = 0; i < output_size; i++){
      for(ssize_t j = 0; j < input_size; j++){
        m_weights[i][j] = distribution(randomNumberGenerator);
      }
    }

    m_biases.resize(output_size);
    for(ssize_t i = 0; i < output_size; i++){
      m_biases[i] = distribution(randomNumberGenerator);
    }

    m_outputs.resize(output_size);
    m_gradients.resize(output_size);
  }

  void forward(const std::vector<double>& inputs) {
    for(ssize_t i = 0; i < m_outputs.size(); i++) {
      m_outputs[i] = 0;
      for(ssize_t j = 0; j < inputs.size(); j++){
        m_outputs[i] += inputs[j] * m_weights[i][j];
      }
      m_outputs[i] += m_biases[i];
      m_outputs[i] = m_activationFunction.m_function(m_outputs[i]);
    }
  }

  void backward(const std::vector<double>& inputs, const std::vector<double>& nextGradients, const std::vector<std::vector<double>>& nextWeights, const ActivationFunctionFunction activationFunctionDerivative = activationFunction_sigmoid_derivative) {
    for(ssize_t i = 0; i < m_gradients.size(); i++) {
      
      m_gradients[i] = 0;
      for(ssize_t j = 0; j < nextGradients.size(); j++){
        m_gradients[i] += nextGradients[j] * nextWeights[j][i];
      }
      m_gradients[i] *= activationFunctionDerivative(m_outputs[i]);

      for(ssize_t j = 0; j < inputs.size(); j++){
        m_weights[i][j] -= m_learningRate * m_gradients[i] * inputs[j];
      }

      m_biases[i] -= m_learningRate * m_gradients[i];

    }
  }

};

class Network {
public:

  std::vector<Layer> m_layers;

  std::vector<double> m_outputs;

  Network(const std::vector<ssize_t>& sizes, const std::vector<ActivationFunction>& activationFunctions) {
    for(ssize_t i = 0; i < sizes.size() - 1; i++){
      const ActivationFunction& activationFunction = (i < activationFunctions.size()) ? activationFunctions[i] : activationFunctionSigmoid;
      m_layers.push_back(Layer(sizes[i], sizes[i + 1], activationFunction));
    }
    m_outputs.resize(sizes.back());
  }

  void evaluate(const std::vector<double>& inputs, std::vector<double>& outputs) {
    m_layers[0].forward(inputs);
    for(ssize_t i = 1; i < m_layers.size(); i++){
      m_layers[i].forward(m_layers[i - 1].m_outputs);
    }
    if(outputs.size() != m_layers.back().m_outputs.size()) {
      outputs.resize(m_layers.back().m_outputs.size());
    }
    for(ssize_t i = 0; i < outputs.size(); i++){
      outputs[i] = m_layers.back().m_outputs[i];
    } 
    //outputs = m_layers.back().m_outputs;
  }

  void train(const std::vector<double>& inputs, const std::vector<double>& targets) {
      
    // Forward pass
    m_layers[0].forward(inputs);
    for(ssize_t i = 1; i < m_layers.size(); i++){
      m_layers[i].forward(m_layers[i - 1].m_outputs);
    }

    // Compute output gradients
    for(ssize_t i = 0; i < m_layers.back().m_gradients.size(); i++){
      m_layers.back().m_gradients[i] = (m_layers.back().m_outputs[i] - targets[i]) * m_layers.back().m_activationFunction.m_derivative(m_layers.back().m_outputs[i]);
    }

    // Backward pass
    for(ssize_t i = m_layers.size() - 2; i >= 0; i--){
      const std::vector<double>& inputValues = (i > 0) ? m_layers[i - 1].m_outputs : inputs;
      std::vector<double>& nextGradients = m_layers[i + 1].m_gradients;
      std::vector<std::vector<double>>& nextWeights = m_layers[i + 1].m_weights;
      m_layers[i].backward(inputValues, nextGradients, nextWeights, m_layers[i].m_activationFunction.m_derivative);
    }

  } 

  double getMeanSquaredError(const std::vector<std::vector<double>>& inputs, const std::vector<std::vector<double>>& targets) {

    if(inputs.empty() || (inputs.size() != targets.size())) {
      return INFINITY;
    }else{

      std::vector<double>& outputs = m_outputs;
      if(outputs.size() != targets[0].size()){
        outputs.resize(targets[0].size());
      }

      double meanSquaredError = 0.0;
      for(ssize_t i = 0; i < inputs.size(); i++){
        evaluate(inputs[i], outputs);
        for(ssize_t j = 0; j < outputs.size(); j++){
          double error = outputs[j] - targets[i][j];
          meanSquaredError += error * error;
        }
      }
      
      meanSquaredError = sqrt(meanSquaredError / (inputs.size() * targets[0].size()));
      
      return meanSquaredError;

    } 

  }

};

struct Color {
  double m_r;
  double m_g;
  double m_b;
  double m_a;
};

typedef std::vector<Color> ColorSet;

struct TrainingSample {
  std::vector<double> m_inputs;
  std::vector<double> m_targets;
};

typedef std::vector<TrainingSample> TrainingSet;
typedef std::vector<ssize_t> TrainingSampleIndices;

void blendBackToFront(Color& target, const Color& source){
  double oneMinusSourceAlpha = 1.0 - source.m_a;
  target.m_r = (target.m_r * oneMinusSourceAlpha) + (source.m_r * source.m_a);
  target.m_g = (target.m_g * oneMinusSourceAlpha) + (source.m_g * source.m_a);
  target.m_b = (target.m_b * oneMinusSourceAlpha) + (source.m_b * source.m_a);
  target.m_a = (target.m_a * oneMinusSourceAlpha) + source.m_a;
} 
  
void blendFrontToBack(Color& target, const Color& source){
  double oneMinusTargetAlpha = 1.0 - target.m_a;
  double weight = oneMinusTargetAlpha * source.m_a;
  target.m_r += source.m_r * weight;
  target.m_g += source.m_g * weight;
  target.m_b += source.m_b * weight;
  target.m_a += weight;  
}

ssize_t getFactorial(ssize_t n){
  ssize_t result = 1;
  for(ssize_t i = 1; i <= n; i++){
    result *= i;
  }
  return result;
}

std::vector<std::vector<ssize_t>> generatePermutations(int size){
  std::vector<ssize_t> sequence(size);
  std::iota(sequence.begin(), sequence.end(), 0);
  std::vector<std::vector<ssize_t>> permutations;
  permutations.reserve(getFactorial(sequence.size()));
  do {
    permutations.emplace_back(sequence);
  } while (std::next_permutation(sequence.begin(), sequence.end()));
  return permutations;
}

#define MAXIMUM_COLOR_COUNT 10

typedef std::array<ssize_t, MAXIMUM_COLOR_COUNT> PermutationIndexCombination; 

void hash_combine(std::size_t& seed, ssize_t value) {
  seed ^= ((std::size_t)value) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct container_hasher {
  template<class T>
  std::size_t operator()(const T& c) const {
    std::size_t seed = 0;
    for(const auto& elem : c) {
      hash_combine(seed, std::hash<typename T::value_type>()(elem));
    }
    return seed;
  }
};

int main() {

  std::vector<ssize_t> sizes = {
    10, // 10 inputs (1x: Average opacity, excluding the front two fragments + 
        //            3x: Average RGB color, excluding the front two fragments + 
        //            3x: Accumulated premultiplied alpha RGB color
        //            3x: Correct OIT RGB color of the two front fragments 
        //            --
        //            10 inputs in total)
    32, // 32 neurons in the first hidden layer
    16, // 16 neurons in the second hidden layer 
    3 // 3 outputs as RGB color
  };
  std::vector<ActivationFunction> activationFunctions = {
    activationFunctionRELU,    // Input layer activation function (not used)
    activationFunctionRELU,    // First hidden layer activation function
    activationFunctionRELU,    // Second hidden layer activation function
    activationFunctionSigmoid  // Output layer activation function
  };
  Network network(sizes, activationFunctions);

  // Compute all possible color combinations in 10 steps per color channel from 0.0 to 1.0 range 
  ColorSet colorSet;
  {
    ssize_t colorValueSteps = 10;
    std::cout << "Computing all possible color combinations in " << colorValueSteps << " steps per color channel from 0.0 to 1.0 range..." << std::endl;
    ssize_t maximalValue = 1 << 24; // 8.24 bit fixed point
    double maximalValueReciprocal = 1.0 / maximalValue;
    ssize_t step = maximalValue / colorValueSteps;
    for(ssize_t r = 0; r <= maximalValue; r += step) {
      for(ssize_t g = 0; g <= maximalValue; g += step) {
        for(ssize_t b = 0; b <= maximalValue; b += step) {
          for(ssize_t a = 0; a <= maximalValue; a += step) {
            colorSet.push_back({ r * maximalValueReciprocal, g * maximalValueReciprocal, b * maximalValueReciprocal, a * maximalValueReciprocal });
          }
        }
      }
    } 
    std::cout << "Color combination count: " << colorSet.size() << std::endl;
  }
  
  // Permutation index combination hash table
  std::unordered_map<PermutationIndexCombination, bool, container_hasher> permutationIndexCombinationHashTable;

  // Initialize training set
  std::cout << "Initializing training set..." << std::endl;
  TrainingSet trainingSet;
  for(ssize_t countColors = 3; countColors <= MAXIMUM_COLOR_COUNT; countColors++) {  
    
    ssize_t countMinusTwo = countColors - 2;

    ssize_t MAXIMUM_PERMUTATION_COUNT = getFactorial(countColors);
    if(MAXIMUM_PERMUTATION_COUNT > 131072) {
      MAXIMUM_PERMUTATION_COUNT = 131072;
    }
    for(ssize_t permutationIndex = 0; permutationIndex < MAXIMUM_PERMUTATION_COUNT; permutationIndex++) {
      
      std::vector<ssize_t> permutation(countColors);
      
      // Generate random permutation, but make sure that it is unique
      {
        PermutationIndexCombination permutationIndexCombination;
        do{
          for(ssize_t i = 0; i < permutation.size(); i++) {
            permutation[i] = i; 
          }
          std::shuffle(permutation.begin(), permutation.end(), randomNumberGenerator);
          for(ssize_t i = 0; i < permutationIndexCombination.size(); i++) {
            permutationIndexCombination[i] = (i < countColors) ? permutation[i] : -1;
          }
        } while((!permutationIndexCombinationHashTable.empty()) && 
                 (permutationIndexCombinationHashTable.find(permutationIndexCombination) != permutationIndexCombinationHashTable.end()));
        permutationIndexCombinationHashTable.emplace(permutationIndexCombination, true);
        for(ssize_t i = 0; i < permutation.size(); i++) {
          permutation[i] = permutationIndexCombination[i];
        }
      }

      // Compute color set for the current permutation for the current colo count
      std::vector<Color> colors(countColors);
      for(ssize_t i = 0; i < colors.size(); i++) {
        colors[i] = colorSet[permutation[i]];
      }

      // Compute average color and alpha
      Color averageColor = { 0.0, 0.0, 0.0, 0.0 };
      for(ssize_t i = 2; i < colors.size(); i++) {
        averageColor.m_r += colors[i].m_r;
        averageColor.m_g += colors[i].m_g;
        averageColor.m_b += colors[i].m_b;
        averageColor.m_a += colors[i].m_a;
      }
      averageColor.m_r /= countMinusTwo;
      averageColor.m_g /= countMinusTwo;
      averageColor.m_b /= countMinusTwo;
      averageColor.m_a /= countMinusTwo;

      // Compute accumulated premultiplied alpha color
      Color accumulatedPremultipliedAlphaColor = { 0.0, 0.0, 0.0, 1.0 };
      for(ssize_t i = 0; i < colors.size(); i++) {
        accumulatedPremultipliedAlphaColor.m_r += colors[i].m_r * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_g += colors[i].m_g * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_b += colors[i].m_b * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_a *= 1.0 - colors[i].m_a;
      }    

      // Compute correct OIT color for the two front fragments
      Color correctOITColor = { 0.0, 0.0, 0.0, 0.0 };
      for(ssize_t i = 0; i < 2; i++) {
        blendFrontToBack(correctOITColor, colors[i]);
      } 

      // Compute correct OIT color for all fragments
      Color totalOITColor = { 0.0, 0.0, 0.0, 0.0 };
      for(ssize_t i = 0; i < colors.size(); i++) {
        blendFrontToBack(totalOITColor, colors[i]);
      }

      // Add training sample
      TrainingSample trainingSample;
      trainingSample.m_inputs.resize(10);
      trainingSample.m_targets.resize(3);
      trainingSample.m_inputs[0] = averageColor.m_a;
      trainingSample.m_inputs[1] = averageColor.m_r;
      trainingSample.m_inputs[2] = averageColor.m_g;
      trainingSample.m_inputs[3] = averageColor.m_b;
      trainingSample.m_inputs[4] = accumulatedPremultipliedAlphaColor.m_r;
      trainingSample.m_inputs[5] = accumulatedPremultipliedAlphaColor.m_g;
      trainingSample.m_inputs[6] = accumulatedPremultipliedAlphaColor.m_b;
      trainingSample.m_inputs[7] = correctOITColor.m_r;
      trainingSample.m_inputs[8] = correctOITColor.m_g;
      trainingSample.m_inputs[9] = correctOITColor.m_b;  
      trainingSample.m_targets[0] = totalOITColor.m_r;
      trainingSample.m_targets[1] = totalOITColor.m_g;
      trainingSample.m_targets[2] = totalOITColor.m_b;
      trainingSet.push_back(trainingSample);
    
    }

  }
  std::cout << "Training sample count: " << trainingSet.size() << std::endl;
  
  // Initialize training sample indices
  std::cout << "Initializing training sample indices..." << std::endl;
  TrainingSampleIndices trainingSampleIndices;
  trainingSampleIndices.resize(trainingSet.size());
  for(ssize_t i = 0; i < trainingSampleIndices.size(); i++) {
    trainingSampleIndices[i] = i;
  }
  
  // Initialize test set
  std::vector<std::vector<double>> testInputs;
  std::vector<std::vector<double>> testTargets;
  for(ssize_t trainingSampleIndex = 0; trainingSampleIndex < trainingSet.size(); trainingSampleIndex++) {
    TrainingSample& trainingSample = trainingSet[trainingSampleIndex];
    testInputs.push_back(trainingSample.m_inputs);
    testTargets.push_back(trainingSample.m_targets);
  }

  // Train the network
  std::cout << "Training the network..." << std::endl;
  ssize_t epochs = 4096;
  double meanSquaredError = network.getMeanSquaredError(testInputs, testTargets);
  for(ssize_t epochIndex = 0; epochIndex < epochs; epochIndex++) {    

    std::cout << "\rEpoch " << epochIndex + 1 << " of " << epochs << "... Mean squared error: " << meanSquaredError << " " << std::flush;
 
    // Shuffle trainingSampleIndices    
    std::shuffle(trainingSampleIndices.begin(), trainingSampleIndices.end(), randomNumberGenerator);
 
    // Train the network with the shuffled training samples
    for(ssize_t trainingSampleIndex : trainingSampleIndices) {
      TrainingSample& trainingSample = trainingSet[trainingSampleIndex];
      network.train(trainingSample.m_inputs, trainingSample.m_targets);
    }

    meanSquaredError = network.getMeanSquaredError(testInputs, testTargets);
    if (meanSquaredError < 1e-5){
      // Good enough, stop training
      break;
    }
    
  }
  std::cout << "\nTraining Done! Final mean squared error: " << meanSquaredError << std::endl;

  // Test the network
  std::cout << "Exporting the network to GLSL float arrays..." << std::endl;
  std::string fileName = "dfaoit_network.glsl";
  std::ofstream file(fileName);
  if(!file.is_open()) {
    std::cerr << "Error: Could not open file \"" << fileName << "\" for writing!" << std::endl;
    return 1;
  }
  
  file << "// dfaoit_network.glsl" << std::endl;
  file << "// This file was automatically generated by dfaoittrain" << std::endl;
  file << std::endl;
  
  for(ssize_t layerIndex = 0; layerIndex < network.m_layers.size(); layerIndex++) {
    
    Layer& layer = network.m_layers[layerIndex];

    file << "const float weights" << layerIndex +1 << "[" << layer.m_weights[0].size() << "][" << layer.m_weights.size() << "] = {" << std::endl;
    for(ssize_t i = 0; i < layer.m_weights[0].size(); i++) {
      file << "  { " << std::endl;
      for(ssize_t j = 0; j < layer.m_weights.size(); j++) {
        file << "    " << layer.m_weights[j][i];
        if(j < layer.m_weights.size() - 1) {
          file << ", ";
        }
        file << std::endl;
      }
      file << "  }";
      if(i < layer.m_weights[0].size() - 1) {
        file << ",";
      }
      file << std::endl;
    }
    file << "};" << std::endl;
    file << std::endl;

    file << "const float biases" << layerIndex + 1 << "[" << layer.m_biases.size() << "] = { " << std::endl;
    for(ssize_t i = 0; i < layer.m_biases.size(); i++) {
      file << "  " << layer.m_biases[i];
      if(i < layer.m_biases.size() - 1) {
        file << ", ";
      }
      file << std::endl;
    }
    file << "};" << std::endl;
    file << std::endl;

  }
  file.close();

  std::cout << "Done!" << std::endl;

  return 0;
}

#if 0
/* GLSL evaluation code (weights1, biases1, weights2, biases2, weights3, biases3): */

float relu(float x){
  return max(0.0, x);
} 

float sigmoid(float x) {
  return 1.0 / (1.0 + exp(-x));
}

vec3 evalulateNetwork(const in float inputValues[10]){
  
  float output1[32];
  for(int i = 0; i < 32; i++) {
    output1[i] = 0.0;
    for(int j = 0; j < 10; j++){
      output1[i] += inputValues[j] * weights1[i][j];
    }
    output1[i] += biases1[i];
    output1[i] = relu(output1[i]);
  }

  float output2[16];
  for(int i = 0; i < 16; i++) {
    output2[i] = 0.0;
    for(int j = 0; j < 32; j++){
      output2[i] += output1[j] * weights2[i][j];
    }
    output2[i] += biases2[i];
    output2[i] = relu(output2[i]);
  }

  vec3 output3;
  for(int i = 0; i < 3; i++) {
    output3[i] = 0.0;
    for(int j = 0; j < 16; j++){
      output3[i] += output2[j] * weights3[i][j];
    }
    output3[i] += biases3[i];
    output3[i] = sigmoid(output3[i]);
  }

  return output3;
}

#endif