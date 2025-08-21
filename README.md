# 📝 Text Mining, Sentiment Analysis & Network Modelling

This project applies **natural language processing (NLP)** and **network analysis** to a corpus of texts spanning three categories:  
- 🎵 Song Lyrics  
- 📖 Philosophy Texts  
- 🍴 Restaurant Reviews  

The aim is to explore clustering, sentiment, and network properties of the documents to gain insights into both linguistic patterns and structural relationships.

---

## 🚀 Project Overview
1. **Preprocessing & Document-Term Matrix (DTM)**  
   - Standard NLP pipeline: tokenisation, stopword removal, stemming, sparsity reduction.  
   - Constructed DTM and exported for analysis.  

2. **Exploratory Analysis & Clustering**  
   - Term frequency exploration.  
   - Cosine distance + Ward’s hierarchical clustering → grouped documents into **3 clusters (by genre)**.  
   - Achieved **classification accuracy ≈ 80–85%** when comparing clusters vs. true labels.  

3. **Sentiment Analysis**  
   - Applied **`SentimentAnalysis` R package**.  
   - Extracted polarity (QDAP), positivity, uncertainty, and word count measures.  
   - Visualised group distributions with boxplots and tested group differences via **Kruskal-Wallis tests**.  

4. **Network Analysis**  
   - Constructed multiple networks:
     - **Document–Document network** (based on cosine similarity).  
     - **Token–Token co-occurrence network**.  
     - **Bipartite Document–Token network**.  
   - Measured **degree, closeness, betweenness, eigenvector centrality**.  
   - Visualised networks with color/size scaling by centrality to highlight influential documents and terms.  

---

## 📊 Key Insights
- Genres clustered meaningfully by **linguistic content**, with restaurant reviews showing the most distinctive sentiment polarity.  
- Lyrics tended toward **higher positivity**, philosophy toward **uncertainty/neutrality**, and reviews balanced between positive and negative tones.  
- Network analysis revealed:
  - Certain **tokens (keywords)** acted as hubs across genres.  
  - Reviews formed **tighter communities**, while philosophy texts were more **dispersed** in token co-occurrence space.  

---

## 🧰 Tech Stack
- **Language**: R  
- **Libraries**: `tm`, `slam`, `SnowballC`, `proxy`, `SentimentAnalysis`, `igraph`, `dplyr`  
- **Techniques**:  
  - Text preprocessing and feature engineering.  
  - Hierarchical clustering and evaluation.  
  - Sentiment polarity and statistical testing.  
  - Social network analysis (centrality measures, bipartite visualisation).  

---

## 📈 Visualisations
- Dendrogram of hierarchical clusters.  
- Sentiment boxplots across genres.  
- Document–Document and Token–Token networks.  
- Bipartite Document–Token graph.  

---

## 🔑 Learning Outcomes
- Built an **end-to-end NLP workflow in R**.  
- Gained hands-on experience in **text clustering, sentiment modelling, and network analysis**.  
- Demonstrated **interpretability** of results through feature importance and centrality metrics.  

---

## 📝 Author
👤 **Brennen Chong**  
Penultimate-year Computer Science student @ Monash University | Data Science & Software Engineering Enthusiast  
