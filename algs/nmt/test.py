import model, model_helper
import tensorflow as tf
import inference, os, time
import utils

hparams = utils.load_hparams('/home/burak/Downloads/nmt_model')

infer_model = model_helper.create_infer_model(model.Model, hparams)

infer_sess = tf.Session(config=utils.get_config_proto(), graph=infer_model.graph)

import train, inference

inference.single_worker_inference(infer_model,
                                  "/home/burak/Downloads/nmt_model/translate.ckpt-12000",
                                  "/tmp/in",
                                  "/tmp/out",
                                  hparams)

