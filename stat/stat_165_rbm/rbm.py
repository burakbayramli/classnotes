import numpy as np
import itertools

class RBM:
  
  def __init__(self, num_hidden, learning_rate,max_epochs, num_visible=10):
    self.num_hidden = num_hidden
    self.num_visible = num_visible
    self.learning_rate = learning_rate
    # Agirlik matrisi W'yi yarat (buyukluk num_visible x num_hidden),
    # bunun icin Gaussian dagilimi kullan, ortalama=0, standart sapma 1. 
    self.weights = 0.1 * np.random.randn(self.num_visible, self.num_hidden)    
    # Egilim (bias) icin ilk satir ve ilk kolona 1 degeri koy
    self.weights = np.insert(self.weights, 0, 0, axis = 0)
    self.weights = np.insert(self.weights, 0, 0, axis = 1)
    self.max_epochs = max_epochs
    
  def fit(self, data):
    """
    Makinayi egit

    Parametreler
    ----------
    data: Her satirin "gorunen" veri oldugu bir matris
    """

    num_examples = data.shape[0]

    # Ilk kolona egilim / meyil (bias) olarak 1 ekle
    data = np.insert(data, 0, 1, axis = 1)

    for epoch in range(self.max_epochs):      
      # Veriyi baz alarak gizli veriyi uret. 
      pos_hidden_activations = np.dot(data, self.weights)
      pos_hidden_probs = self._logistic(pos_hidden_activations)
      pos_hidden_states = pos_hidden_probs > \
          np.random.rand(num_examples, self.num_hidden + 1)

      tmp = np.array(pos_hidden_states).astype(float)
      pos_visible_states = self.run_hidden(tmp[:,1:])
        
      # Dikkat, baglantilari hesaplarken h tabakasinin aktivasyon
      # olasiliklarini kullaniyoruz h'nin kendi degerlerini (0/1)
      # kullanmiyoruz. Bunu da yapabilirdik, daha fazla detay icin
      # Hinton'un "A Practical Guide to Training Restricted Boltzmann
      # Machines" makalesine bakilabilir
      pos_associations = np.dot(data.T, pos_hidden_probs)

      # Simdi gorunen veriyi gizli veriyi baz alip tekrar uret
      neg_visible_activations = np.dot(pos_hidden_states, self.weights.T)
      neg_visible_probs = self._logistic(neg_visible_activations)
      neg_visible_probs[:,0] = 1 # Fix the bias unit.
      neg_hidden_activations = np.dot(neg_visible_probs, self.weights)
      neg_hidden_probs = self._logistic(neg_hidden_activations)

      # Yine ayni durum, aktivasyon olasiliklari kullaniliyor
      neg_associations = np.dot(neg_visible_probs.T, neg_hidden_probs)

      # Agirliklari guncelle
      self.weights += self.learning_rate * \
          ((pos_associations - neg_associations) / num_examples)

      error = np.sum((data - neg_visible_probs) ** 2)
      
  def run_visible(self, data):
    """
    RBM'in egitilmis olduguna farz ederek, gorunen veri uzerinde
    RBM'i islet, ve h icin bir orneklem al

    Parametreler
    ----------
    data: Her satirin gorunen veri oldugu bir matris
    
    Returns
    -------
    hidden_states: data icindeki her satira tekabul eden gizli h verisi
    """
    num_examples = data.shape[0]
    
    hidden_states = np.ones((num_examples, self.num_hidden + 1))
    
    data = np.insert(data, 0, 1, axis = 1)

    hidden_activations = np.dot(data, self.weights)
    hidden_probs = self._logistic(hidden_activations)
    hidden_states[:,:] = hidden_probs > \
        np.random.rand(num_examples, self.num_hidden + 1)  
    hidden_states = hidden_states[:,1:]
    return hidden_states

          
  def run_hidden(self, data):
    """
    run_visible'a benzer, sadece gizli veri icin gorunen veri uret
    """

    num_examples = data.shape[0]

    visible_states = np.ones((num_examples, self.num_visible + 1))

    data = np.insert(data, 0, 1, axis = 1)

    visible_activations = np.dot(data, self.weights.T)
    visible_probs = self._logistic(visible_activations)
    visible_states[:,:] = visible_probs > \
        np.random.rand(num_examples, self.num_visible + 1)

    visible_states = visible_states[:,1:]
    return visible_states
  
  def _logistic(self, x):
    return 1.0 / (1 + np.exp(-x))

        
if __name__ == "__main__":    
    import numpy as np
    X = np.array([[0, 0, 0], [0, 1, 1], [1, 0, 1], [1, 1, 1]])
    model = RBM(num_hidden=2,learning_rate=0.1,max_epochs=10,num_visible=3)
    model.fit(X)
    print model.weights
