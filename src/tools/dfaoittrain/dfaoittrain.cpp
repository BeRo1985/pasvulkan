#include <iostream>
#include <algorithm>
#include <cstdint>
#include <vector>
#include <cmath>
#include <random>

inline static double sigmoid(double x) {
  return 1.0 / (1.0 + exp(-x));
}

inline static double sigmoid_derivative(double x) {
  return x * (1.0 - x);
}

class Layer {
public:
  std::vector<double> m_outputs;
  std::vector<double> m_gradients;
  std::vector<std::vector<double>> m_weights;
  std::vector<double> m_biases; 
  double m_learningRate = 0.1;

  Layer(int input_size, int output_size) {
    // Initialize weights and biases with random values between -1 and 1
    std::random_device randomDevice;
    std::mt19937 randomNumberGenerator(randomDevice());
    std::uniform_real_distribution<> distribution(-1, 1);

    m_weights.resize(output_size, std::vector<double>(input_size));
    for(size_t i = 0; i < output_size; i++){
      for(size_t j = 0; j < input_size; j++){
        m_weights[i][j] = distribution(randomNumberGenerator);
      }
    }

    m_biases.resize(output_size);
    for(size_t i = 0; i < output_size; i++){
      m_biases[i] = distribution(randomNumberGenerator);
    }

    m_outputs.resize(output_size);
    m_gradients.resize(output_size);
  }

  void forward(const std::vector<double>& inputs) {
    for(size_t i = 0; i < m_outputs.size(); i++) {
      m_outputs[i] = 0;
      for(size_t j = 0; j < inputs.size(); j++){
        m_outputs[i] += inputs[j] * m_weights[i][j];
      }
      m_outputs[i] += m_biases[i];
      m_outputs[i] = sigmoid(m_outputs[i]);
    }
  }

  void backward(const std::vector<double>& inputs, const std::vector<double>& next_gradients, const std::vector<std::vector<double>>& next_weights) {
    for(size_t i = 0; i < m_gradients.size(); i++) {
      
      m_gradients[i] = 0;
      for(size_t j = 0; j < next_gradients.size(); j++){
        m_gradients[i] += next_gradients[j] * next_weights[j][i];
      }
      m_gradients[i] *= sigmoid_derivative(m_outputs[i]);

      for(size_t j = 0; j < inputs.size(); j++){
        m_weights[i][j] -= m_learningRate * m_gradients[i] * inputs[j];
      }

      m_biases[i] -= m_learningRate * m_gradients[i];

    }
  }

};

class Network {
public:
  std::vector<Layer> m_layers;

  Network(const std::vector<size_t>& sizes) {
    for(size_t i=0; i<sizes.size() - 1; i++){
      m_layers.push_back(Layer(sizes[i], sizes[i+1]));
    }
  }

  void evaluate(const std::vector<double>& inputs, std::vector<double>& outputs) {
    m_layers[0].forward(inputs);
    for(size_t i = 1; i<m_layers.size(); i++){
      m_layers[i].forward(m_layers[i-1].m_outputs);
    }
    outputs = m_layers.back().m_outputs;
  }

  void train(const std::vector<double>& inputs, const std::vector<double>& targets) {
   
    // Forward pass
    m_layers[0].forward(inputs);
    for(size_t i = 1; i<m_layers.size(); i++){
      m_layers[i].forward(m_layers[i-1].m_outputs);
    }

    // Compute output gradients
    for(int i = 0; i < m_layers.back().m_gradients.size(); i++){
      m_layers.back().m_gradients[i] = (m_layers.back().m_outputs[i] - targets[i]) * sigmoid_derivative(m_layers.back().m_outputs[i]);
    }

    // Backward pass
    for(size_t i = m_layers.size() - 2; i >= 0; i--){
      m_layers[i].backward((i > 0) ? m_layers[i - 1].m_outputs:inputs, m_layers[i + 1].m_gradients, m_layers[i + 1].m_weights);
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
typedef std::vector<size_t> TrainingSampleIndices;

TrainingSet trainingSet;
TrainingSampleIndices trainingSampleIndices;
ColorSet colorSet;
std::vector<ColorSet> colorSetPermutations;

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

size_t factorial(size_t n){
  if (n == 0) {
    return 1;
  }
  return n * factorial(n - 1);
}

std::vector<std::vector<int>> generatePermutations(int size){
  std::vector<int> sequence(size);
  std::iota(sequence.begin(), sequence.end(), 0);
  std::vector<std::vector<int>> permutations;
  permutations.reserve(factorial(sequence.size()));
  do {
    permutations.emplace_back(sequence);
  } while (std::next_permutation(sequence.begin(), sequence.end()));
  return permutations;
}

std::vector<std::vector<int>> permutationIndices;

int main() {

  std::vector<size_t> sizes = {
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
  Network network(sizes);

  // Compute all possible color combinations in 10 steps per color channel from 0.0 to 1.0 range 
  int32_t percentStep = 10;
  for(int32_t percentR = 0; percentR <= 100; percentR += percentStep) {
    for(int32_t percentG = 0; percentG <= 100; percentG += percentStep) {
      for(int32_t percentB = 0; percentB <= 100; percentB += percentStep) {
        for(int32_t percentA = 0; percentA <= 100; percentA += percentStep) {
          colorSet.push_back({ percentR * 0.01, percentG * 0.01, percentB * 0.01, percentA * 0.01 });
        }
      }
    }
  } 

  // Generate permutation indices
  permutationIndices = generatePermutations(colorSet.size());

  // Initialize training set
  for(size_t countColors = 3; countColors <= 16; countColors++) {  
    
    size_t countMinusTwo = countColors - 2;

    for(size_t permutationIndex = 0; permutationIndex < permutationIndices.size(); permutationIndex++) {
      
      std::vector<int>& permutation = permutationIndices[permutationIndex];

      std::vector<Color> colors(countColors);
      for(size_t i = 0; i < colors.size(); i++) {
        colors[i] = colorSet[permutation[i]];
      }

      // Compute average color and alpha
      Color averageColor = { 0.0, 0.0, 0.0, 0.0 };
      for(size_t i = 2; i < colors.size(); i++) {
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
      for(size_t i = 0; i < colors.size(); i++) {
        accumulatedPremultipliedAlphaColor.m_r += colors[i].m_r * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_g += colors[i].m_g * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_b += colors[i].m_b * colors[i].m_a;
        accumulatedPremultipliedAlphaColor.m_a *= 1.0 - colors[i].m_a;
      }    

      // Compute correct OIT color for the two front fragments
      Color correctOITColor = { 0.0, 0.0, 0.0, 0.0 };
      for(size_t i = 0; i < 2; i++) {
        blendFrontToBack(correctOITColor, colors[i]);
      } 

      // Compute correct OIT color for all fragments
      Color totalOITColor = { 0.0, 0.0, 0.0, 0.0 };
      for(size_t i = 0; i < colors.size(); i++) {
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
  
  // Initialize training sample indices
  trainingSampleIndices.resize(trainingSet.size());
  for(size_t i = 0; i < trainingSampleIndices.size(); i++) {
    trainingSampleIndices[i] = i;
  }
  
  std::random_device randomDevice;
  std::mt19937 randomNumberGenerator(randomDevice());

  // Train the network
  size_t epochs = 4096;
  for(size_t epochIndex = 0; epochIndex < epochs; epochIndex++) {    
 
    // Shuffle trainingSampleIndices    
    std::shuffle(trainingSampleIndices.begin(), trainingSampleIndices.end(), randomNumberGenerator);
 
    // Train the network with the shuffled training samples
    for(size_t trainingSampleIndex : trainingSampleIndices) {
      TrainingSample& trainingSample = trainingSet[trainingSampleIndex];
      network.train(trainingSample.m_inputs, trainingSample.m_targets);
    }
    
  }

  return 0;
}