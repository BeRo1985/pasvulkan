// This tool trains a neural network to approximate the correct OIT color for a given set of colors for usage in a fragment shader.
// The neural network is trained with all possible color combinations for a given colo count. 

// Copyright (C) 2023 by Benjamin 'BeRo' Rosseaux - Licensed under the zlib license

#include <ostream>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <vector>
#include <cmath>
#include <random>
#include <unordered_map>
#include <array>
#include "pcg_random.hpp"
#include <stdexcept>
#include <iomanip>
#include <memory>
#include <string>

#include <immintrin.h>

#define USE_OPENMP 1
#define ASSERT_MSG(cond, msg) { if (!(cond)) throw std::runtime_error("Assertion (" #cond ") failed at line " + std::to_string(__LINE__) + "! Msg '" + std::string(msg) + "'."); }
#define ASSERT(cond) ASSERT_MSG(cond, "")
#if defined(_MSC_VER)
    #define IS_MSVC 1
#else
    #define IS_MSVC 0
#endif

#if USE_OPENMP
    #include <omp.h>
#endif

template <typename T, std::size_t N>
class AlignmentAllocator {
  public:
    typedef T value_type;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;
    typedef T * pointer;
    typedef const T * const_pointer;
    typedef T & reference;
    typedef const T & const_reference;

  public:
    inline AlignmentAllocator() throw() {}
    template <typename T2> inline AlignmentAllocator(const AlignmentAllocator<T2, N> &) throw() {}
    inline ~AlignmentAllocator() throw() {}
    inline pointer adress(reference r) { return &r; }
    inline const_pointer adress(const_reference r) const { return &r; }
    inline pointer allocate(size_type n);
    inline void deallocate(pointer p, size_type);
    inline void construct(pointer p, const value_type & wert);
    inline void destroy(pointer p) { p->~value_type(); }
    inline size_type max_size() const throw() { return size_type(-1) / sizeof(value_type); }
    template <typename T2> struct rebind { typedef AlignmentAllocator<T2, N> other; };
    bool operator!=(const AlignmentAllocator<T, N> & other) const { return !(*this == other); }
    bool operator==(const AlignmentAllocator<T, N> & other) const { return true; }
};

template <typename T, std::size_t N>
inline typename AlignmentAllocator<T, N>::pointer AlignmentAllocator<T, N>::allocate(size_type n) {
    #if IS_MSVC
        auto p = (pointer)_aligned_malloc(n * sizeof(value_type), N);
    #else
        auto p = (pointer)std::aligned_alloc(N, n * sizeof(value_type));
    #endif
    ASSERT(p);
    return p;
}
template <typename T, std::size_t N>
inline void AlignmentAllocator<T, N>::deallocate(pointer p, size_type) {
    #if IS_MSVC
        _aligned_free(p);
    #else
        std::free(p);
    #endif
}
template <typename T, std::size_t N>
inline void AlignmentAllocator<T, N>::construct(pointer p, const value_type & wert) {
    new (p) value_type(wert);
}

template <typename T>
using AlignedVector = std::vector<T, AlignmentAllocator<T, 64>>;

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
  return activationFunction_sigmoid(x) * (1.0 - activationFunction_sigmoid(x));
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
  AlignedVector<double> m_outputs;
  AlignedVector<double> m_gradients;
  AlignedVector<double> m_weights;
  AlignedVector<double> m_biases; 
  ActivationFunction m_activationFunction;
  ssize_t m_weights_rows;
  ssize_t m_weights_cols;
  double m_learningRate = 0.00001;

  Layer(ssize_t input_size, ssize_t output_size, const ActivationFunction& activationFunction = activationFunctionSigmoid) : m_activationFunction(activationFunction) {

    // Initialize weights and biases with random values between -1 and 1
    std::uniform_real_distribution<> distribution(-1, 1);

    m_weights_rows = output_size;
    m_weights_cols = input_size;

    m_weights.resize(m_weights_rows * m_weights_cols);    
    for(ssize_t i = 0, j = m_weights_rows * m_weights_cols; i < j; i++){
      m_weights[i] = distribution(randomNumberGenerator);
    }

    m_biases.resize(m_weights_rows);
    for(ssize_t i = 0; i < m_weights_rows; i++){
      m_biases[i] = distribution(randomNumberGenerator);
    }

    m_outputs.resize(m_weights_rows);
    m_gradients.resize(m_weights_rows);
  }

  inline double getWeight(ssize_t row, ssize_t col) const {
    return m_weights[(row * m_weights_cols) + col];
  }

  inline void setWeight(ssize_t row, ssize_t col, double weight) {
    m_weights[(row * m_weights_cols) + col] = weight;
  }

  void forward(const AlignedVector<double>& inputs) {
   ssize_t rows = m_weights_rows, cols = m_weights_cols; 
    for(ssize_t i = 0; i < rows; i++) {
      double output = 0.0;
      ssize_t j = 0;
#ifdef __AVX2__      
      for(; (j + 3) < cols; j += 4){
        __m256d t = _mm256_mul_pd(_mm256_loadu_pd(&inputs[j]), _mm256_loadu_pd(&m_weights[(i * cols) + j]));
        t = _mm256_hadd_pd(t, t);
        output += t[0] + t[2];
      }
#endif
#ifdef __SSE2__
      for(; (j + 1) < cols; j += 2){
        __m128d t = _mm_mul_pd(_mm_loadu_pd(&inputs[j]), _mm_loadu_pd(&m_weights[(i * cols) + j]));
        t = _mm_hadd_pd(t, t);
        output += t[0];
      }      
#endif
      for(; j < cols; j++){
        output += inputs[j] * m_weights[(i * cols) + j];
      }
      m_outputs[i] = m_activationFunction.m_function(output + m_biases[i]);
    }    
  }

  void backward(const AlignedVector<double>& inputs, const Layer& nextLayer, const ActivationFunctionFunction activationFunctionDerivative = activationFunction_sigmoid_derivative) {
    for(ssize_t i = 0; i < m_weights_rows; i++) {
      
      double gradient = 0.0;
      {
        ssize_t j = 0, rows = nextLayer.m_weights_rows, cols = nextLayer.m_weights_cols; 
#ifdef __AVX2__
        for(; (j + 3) < rows; j += 4){
          __m256d t = _mm256_mul_pd(_mm256_loadu_pd(&nextLayer.m_gradients[j]), 
                                    _mm256_set_pd(nextLayer.m_weights[((j + 3) * cols) + i], 
                                                  nextLayer.m_weights[((j + 2) * cols) + i], 
                                                  nextLayer.m_weights[((j + 1) * cols) + i], 
                                                  nextLayer.m_weights[((j + 0) * cols) + i]));
          t = _mm256_hadd_pd(t, t);
          gradient += t[0] + t[2];
        }  
#endif
#ifdef __SSE2__
        for(; (j + 1) < rows; j += 2){
          __m128d t = _mm_mul_pd(_mm_loadu_pd(&nextLayer.m_gradients[j]), 
                                 _mm_set_pd(nextLayer.m_weights[((j + 1) * cols) + i], 
                                            nextLayer.m_weights[((j + 0) * cols) + i]));
          t = _mm_hadd_pd(t, t);
          gradient += t[0];
        }
#endif
        for(; j < rows; j++){
          gradient += nextLayer.m_gradients[j] * nextLayer.m_weights[(j * cols) + i];
        }
        m_gradients[i] = gradient *= activationFunctionDerivative(m_outputs[i]);
      }

      double gradientMulLearningRate = gradient * m_learningRate;

      {
        ssize_t j = 0, cols = m_weights_cols;
#ifdef __AVX2__
        {
          __m256d gradientMulLearningRateSIMD = _mm256_set1_pd(gradientMulLearningRate);
          for(; (j + 3) < cols; j += 4){
            _mm256_storeu_pd(&m_weights[(i * cols) + j], _mm256_sub_pd(_mm256_loadu_pd(&m_weights[(i * cols) + j]), _mm256_mul_pd(_mm256_loadu_pd(&inputs[j]), gradientMulLearningRateSIMD)));
          } 
        }
#endif
#ifdef __SSE2__
        {
          __m128d gradientMulLearningRateSIMD = _mm_set1_pd(gradientMulLearningRate);
          for(; (j + 1) < cols; j += 2){
            _mm_storeu_pd(&m_weights[(i * cols) + j], _mm_sub_pd(_mm_loadu_pd(&m_weights[(i * cols) + j]), _mm_mul_pd(_mm_loadu_pd(&inputs[j]), gradientMulLearningRateSIMD)));
          } 
        }
#endif
        for(; j < m_weights_cols; j++){
          m_weights[(i * m_weights_cols) + j] -= gradientMulLearningRate * inputs[j];
        }
      } 

      m_biases[i] -= gradientMulLearningRate;

    }
  }

};

class Network {
public:

  AlignedVector<Layer> m_layers;

  AlignedVector<double> m_outputs;

  Network(const AlignedVector<ssize_t>& sizes, const AlignedVector<ActivationFunction>& activationFunctions) {
    for(ssize_t i = 0; i < sizes.size() - 1; i++){
      const ActivationFunction& activationFunction = (i < activationFunctions.size()) ? activationFunctions[i] : activationFunctionSigmoid;
      m_layers.push_back(Layer(sizes[i], sizes[i + 1], activationFunction));
    }
    m_outputs.resize(sizes.back());
  }

  void evaluate(const AlignedVector<double>& inputs, AlignedVector<double>& outputs) {
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

  void train(const AlignedVector<double>& inputs, const AlignedVector<double>& targets) {
      
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
      const AlignedVector<double>& inputValues = (i > 0) ? m_layers[i - 1].m_outputs : inputs;
      m_layers[i].backward(inputValues, m_layers[i + 1], m_layers[i].m_activationFunction.m_derivative);
    }

  } 

  double getMeanSquaredError(const AlignedVector<AlignedVector<double>>& inputs, const AlignedVector<AlignedVector<double>>& targets) {

    if(inputs.empty() || (inputs.size() != targets.size())) {
      return INFINITY;
    }else{
        
      AlignedVector<double>& outputs = m_outputs;
      if(outputs.size() != targets[0].size()){
        outputs.resize(targets[0].size());
      }

      double meanSquaredError = 0.0;
      for(ssize_t i = 0; i < inputs.size(); i++){
        evaluate(inputs[i], outputs);
        ssize_t j = 0, k = outputs.size();
#ifdef __AVX2__
        for(; (j + 3) < k; j += 4){
          __m256d t = _mm256_sub_pd(_mm256_loadu_pd(&outputs[j]), _mm256_loadu_pd(&targets[i][j]));
          t = _mm256_mul_pd(t, t);
          t = _mm256_hadd_pd(t, t);
          meanSquaredError += t[0] + t[2];
        }
#endif
#ifdef __SSE2__
        for(; (j + 1) < k; j += 2){
          __m128d t = _mm_sub_pd(_mm_loadu_pd(&outputs[j]), _mm_loadu_pd(&targets[i][j]));
          t = _mm_mul_pd(t, t);
          t = _mm_hadd_pd(t, t);
          meanSquaredError += t[0];
        }
#endif
        for(; j < k; j++){
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

typedef AlignedVector<Color> ColorSet;

struct TrainingSample {
  AlignedVector<double> m_inputs;
  AlignedVector<double> m_targets;
};

typedef AlignedVector<TrainingSample> TrainingSet;
typedef AlignedVector<ssize_t> TrainingSampleIndices;

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

AlignedVector<AlignedVector<ssize_t>> generatePermutations(int size){
  AlignedVector<ssize_t> sequence(size);
  std::iota(sequence.begin(), sequence.end(), 0);
  AlignedVector<AlignedVector<ssize_t>> permutations;
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

  AlignedVector<ssize_t> sizes = {
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
  AlignedVector<ActivationFunction> activationFunctions = {
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
      
      AlignedVector<ssize_t> permutation(countColors);
      
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
      AlignedVector<Color> colors(countColors);
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
  AlignedVector<AlignedVector<double>> testInputs;
  AlignedVector<AlignedVector<double>> testTargets;
  for(ssize_t trainingSampleIndex = 0; trainingSampleIndex < trainingSet.size(); trainingSampleIndex++) {
    TrainingSample& trainingSample = trainingSet[trainingSampleIndex];
    testInputs.push_back(trainingSample.m_inputs);
    testTargets.push_back(trainingSample.m_targets);
  }

  // Train the network
  std::cout << "Training the network..." << std::endl;
  ssize_t epochs = 8192;
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
    if (meanSquaredError < 1e-6){
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

    file << "const float weights" << layerIndex +1 << "[" << layer.m_weights_cols << "][" << layer.m_weights_rows << "] = {" << std::endl;
    for(ssize_t i = 0; i < layer.m_weights_cols; i++) {
      file << "  { " << std::endl;
      for(ssize_t j = 0; j < layer.m_weights_rows; j++) {
        file << "    " << std::setprecision(std::numeric_limits<double>::digits10 + 2) << layer.m_weights[(j * layer.m_weights_cols) + i];
        if(j < layer.m_weights.size() - 1) {
          file << ", ";
        }
        file << std::endl;
      }
      file << "  }";
      if(i < layer.m_weights_rows - 1) {
        file << ",";
      }
      file << std::endl;
    }
    file << "};" << std::endl;
    file << std::endl;

    file << "const float biases" << layerIndex + 1 << "[" << layer.m_biases.size() << "] = { " << std::endl;
    for(ssize_t i = 0; i < layer.m_biases.size(); i++) {
      file << "  " << std::setprecision(std::numeric_limits<double>::digits10 + 2) << layer.m_biases[i];
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