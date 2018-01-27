import utils
hparams = utils.load_hparams('/home/burak/Downloads/nmt_model')

import model, model_helper
model_creator = model.Model
train_model = model_helper.create_train_model(model_creator, hparams)
eval_model = model_helper.create_eval_model(model_creator, hparams)
infer_model = model_helper.create_infer_model(model_creator, hparams)

import tensorflow as tf
import inference, os, time
log_device_placement = hparams.log_device_placement

config_proto = utils.get_config_proto(
    log_device_placement=log_device_placement,
    num_intra_threads=hparams.num_intra_threads,
    num_inter_threads=hparams.num_inter_threads)

target_session=""
infer_sess = tf.Session(
    target=target_session, config=config_proto, graph=infer_model.graph)

import train, inference

inference.single_worker_inference(infer_model,
                                  "/home/burak/Downloads/nmt_model/translate.ckpt-11000",
                                  "/tmp/in",
                                  "/tmp/out",
                                  hparams)

