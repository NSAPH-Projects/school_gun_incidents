#!/bin/bash

#SBATCH -p shared
#SBATCH --mem=80G
#SBATCH --nodes=1
#SBATCH -o logs/causal_analyses_batch_log.txt
#SBATCH -e logs/causal_analyses_batch_log.txt

echo "**** Job starts ****"
date +"%Y-%m-%d %T"
echo "**** JHPCE info ****"
echo "User: ${USER}"
echo "Job id: ${SLURM_JOB_ID}"
echo "Job name: ${SLURM_JOB_NAME}"
echo "Node name: ${SLURMD_NODENAME}"

module load conda_R/4.3.x
Rscript causal_analyses_with_gee.R

echo "**** Job ends ****"
date
