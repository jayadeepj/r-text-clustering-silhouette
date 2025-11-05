# Silhouette Threshold Based Text Clustering for Log Analysis

This repository contains the code implementation for the research paper:

> **Jayadeep, J.** (2017). *Silhouette Threshold Based Text Clustering for Log Analysis.*  
> *International Journal of Data Mining Techniques and Applications*, 6(1), 17–25.

---

## Overview

The project explores **document clustering techniques** for the **automated analysis and categorization of system logs** from multiple sources.  
It introduces a **Silhouette Threshold-based approach** to dynamically identify whether a log entry belongs to an existing or new cluster, improving log organization, monitoring, and automated recommendations.

---

### Key Highlights

- Proposes a **Silhouette Threshold-based clustering method** for log analysis.  
- Compares traditional algorithms like **K-Means**, **Hierarchical**, and **DBSCAN**.  
- Evaluates clustering performance using **Entropy**, **Purity**, and **Silhouette Index**.  
- Introduces **custom distance measures** for new log event classification.  
- Aims to improve **real-time log mapping** and **automated issue resolution**.

---

## Methodology

1. **Data Preprocessing**  
   - Log parsing, tokenization, and TF-IDF vectorization.  

2. **Clustering Algorithms**  
   - K-Means, Hierarchical, DBSCAN, and Silhouette Threshold-based clustering.  

3. **Evaluation Metrics**  
   - **Entropy** – cluster homogeneity  
   - **Purity** – cluster accuracy  
   - **Silhouette Index** – cohesion and separation  

4. **Dynamic Thresholding**  
   - Determines cluster membership for incoming logs using the Silhouette value.

---

## Citation

```bibtex
@article{jayadeep2017silhouette,
  title={Silhouette Threshold Based Text Clustering for Log Analysis},
  author={Jayadeep, J},
  journal={Int'l J. Data Mining Techniques and Applications},
  volume={6},
  number={1},
  pages={17--25},
  year={2017}
}
