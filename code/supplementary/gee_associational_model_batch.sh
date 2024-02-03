#!/bin/bash

#SBATCH -p shared
#SBATCH --mem=80G
#SBATCH --nodes=1
#SBATCH -o logs/gee_associational_log.txt
#SBATCH -e logs/gee_associational_log.txt

echo "**** Job starts ****"
date +"%Y-%m-%d %T"
echo "**** JHPCE info ****"
echo "User: ${USER}"
echo "Job id: ${SLURM_JOB_ID}"
echo "Job name: ${SLURM_JOB_NAME}"
echo "Node name: ${SLURMD_NODENAME}"

module load conda_R/4.3.x
Rscript gee_associational_model.R

echo "**** Job ends ****"
date
