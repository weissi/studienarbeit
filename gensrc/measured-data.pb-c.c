/* Generated by the protocol buffer compiler.  DO NOT EDIT! */

/* Do not generate deprecated warnings for self */
#ifndef PROTOBUF_C_NO_DEPRECATED
#define PROTOBUF_C_NO_DEPRECATED
#endif

#include "measured-data.pb-c.h"
void   measured_data__init
                     (MeasuredData         *message)
{
  static MeasuredData init_value = MEASURED_DATA__INIT;
  *message = init_value;
}
size_t measured_data__get_packed_size
                     (const MeasuredData *message)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &measured_data__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t measured_data__pack
                     (const MeasuredData *message,
                      uint8_t       *out)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &measured_data__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t measured_data__pack_to_buffer
                     (const MeasuredData *message,
                      ProtobufCBuffer *buffer)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &measured_data__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
MeasuredData *
       measured_data__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (MeasuredData *)
     protobuf_c_message_unpack (&measured_data__descriptor,
                                allocator, len, data);
}
void   measured_data__free_unpacked
                     (MeasuredData *message,
                      ProtobufCAllocator *allocator)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &measured_data__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   data_set__init
                     (DataSet         *message)
{
  static DataSet init_value = DATA_SET__INIT;
  *message = init_value;
}
size_t data_set__get_packed_size
                     (const DataSet *message)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_set__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t data_set__pack
                     (const DataSet *message,
                      uint8_t       *out)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_set__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t data_set__pack_to_buffer
                     (const DataSet *message,
                      ProtobufCBuffer *buffer)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_set__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
DataSet *
       data_set__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (DataSet *)
     protobuf_c_message_unpack (&data_set__descriptor,
                                allocator, len, data);
}
void   data_set__free_unpacked
                     (DataSet *message,
                      ProtobufCAllocator *allocator)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_set__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
void   data_points__init
                     (DataPoints         *message)
{
  static DataPoints init_value = DATA_POINTS__INIT;
  *message = init_value;
}
size_t data_points__get_packed_size
                     (const DataPoints *message)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_points__descriptor);
  return protobuf_c_message_get_packed_size ((const ProtobufCMessage*)(message));
}
size_t data_points__pack
                     (const DataPoints *message,
                      uint8_t       *out)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_points__descriptor);
  return protobuf_c_message_pack ((const ProtobufCMessage*)message, out);
}
size_t data_points__pack_to_buffer
                     (const DataPoints *message,
                      ProtobufCBuffer *buffer)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_points__descriptor);
  return protobuf_c_message_pack_to_buffer ((const ProtobufCMessage*)message, buffer);
}
DataPoints *
       data_points__unpack
                     (ProtobufCAllocator  *allocator,
                      size_t               len,
                      const uint8_t       *data)
{
  return (DataPoints *)
     protobuf_c_message_unpack (&data_points__descriptor,
                                allocator, len, data);
}
void   data_points__free_unpacked
                     (DataPoints *message,
                      ProtobufCAllocator *allocator)
{
  PROTOBUF_C_ASSERT (message->base.descriptor == &data_points__descriptor);
  protobuf_c_message_free_unpacked ((ProtobufCMessage*)message, allocator);
}
static const ProtobufCFieldDescriptor measured_data__field_descriptors[4] =
{
  {
    "samplingRate",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_DOUBLE,
    0,   /* quantifier_offset */
    PROTOBUF_C_OFFSETOF(MeasuredData, samplingrate),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "channels",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_UINT32,
    0,   /* quantifier_offset */
    PROTOBUF_C_OFFSETOF(MeasuredData, channels),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "hasExternalData",
    3,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_BOOL,
    0,   /* quantifier_offset */
    PROTOBUF_C_OFFSETOF(MeasuredData, hasexternaldata),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "inlineData",
    4,
    PROTOBUF_C_LABEL_REPEATED,
    PROTOBUF_C_TYPE_MESSAGE,
    PROTOBUF_C_OFFSETOF(MeasuredData, n_inlinedata),
    PROTOBUF_C_OFFSETOF(MeasuredData, inlinedata),
    &data_set__descriptor,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned measured_data__field_indices_by_name[] = {
  1,   /* field[1] = channels */
  2,   /* field[2] = hasExternalData */
  3,   /* field[3] = inlineData */
  0,   /* field[0] = samplingRate */
};
static const ProtobufCIntRange measured_data__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 4 }
};
const ProtobufCMessageDescriptor measured_data__descriptor =
{
  PROTOBUF_C_MESSAGE_DESCRIPTOR_MAGIC,
  "MeasuredData",
  "MeasuredData",
  "MeasuredData",
  "",
  sizeof(MeasuredData),
  4,
  measured_data__field_descriptors,
  measured_data__field_indices_by_name,
  1,  measured_data__number_ranges,
  (ProtobufCMessageInit) measured_data__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor data_set__field_descriptors[3] =
{
  {
    "timeSecs",
    1,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_INT64,
    0,   /* quantifier_offset */
    PROTOBUF_C_OFFSETOF(DataSet, timesecs),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "timeNanosecs",
    2,
    PROTOBUF_C_LABEL_REQUIRED,
    PROTOBUF_C_TYPE_INT64,
    0,   /* quantifier_offset */
    PROTOBUF_C_OFFSETOF(DataSet, timenanosecs),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "channelData",
    3,
    PROTOBUF_C_LABEL_REPEATED,
    PROTOBUF_C_TYPE_MESSAGE,
    PROTOBUF_C_OFFSETOF(DataSet, n_channeldata),
    PROTOBUF_C_OFFSETOF(DataSet, channeldata),
    &data_points__descriptor,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned data_set__field_indices_by_name[] = {
  2,   /* field[2] = channelData */
  1,   /* field[1] = timeNanosecs */
  0,   /* field[0] = timeSecs */
};
static const ProtobufCIntRange data_set__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 3 }
};
const ProtobufCMessageDescriptor data_set__descriptor =
{
  PROTOBUF_C_MESSAGE_DESCRIPTOR_MAGIC,
  "DataSet",
  "DataSet",
  "DataSet",
  "",
  sizeof(DataSet),
  3,
  data_set__field_descriptors,
  data_set__field_indices_by_name,
  1,  data_set__number_ranges,
  (ProtobufCMessageInit) data_set__init,
  NULL,NULL,NULL    /* reserved[123] */
};
static const ProtobufCFieldDescriptor data_points__field_descriptors[2] =
{
  {
    "dataPoints",
    1,
    PROTOBUF_C_LABEL_REPEATED,
    PROTOBUF_C_TYPE_DOUBLE,
    PROTOBUF_C_OFFSETOF(DataPoints, n_datapoints),
    PROTOBUF_C_OFFSETOF(DataPoints, datapoints),
    NULL,
    NULL,
    1,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
  {
    "channel",
    2,
    PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_TYPE_UINT32,
    PROTOBUF_C_OFFSETOF(DataPoints, has_channel),
    PROTOBUF_C_OFFSETOF(DataPoints, channel),
    NULL,
    NULL,
    0,            /* packed */
    0,NULL,NULL    /* reserved1,reserved2, etc */
  },
};
static const unsigned data_points__field_indices_by_name[] = {
  1,   /* field[1] = channel */
  0,   /* field[0] = dataPoints */
};
static const ProtobufCIntRange data_points__number_ranges[1 + 1] =
{
  { 1, 0 },
  { 0, 2 }
};
const ProtobufCMessageDescriptor data_points__descriptor =
{
  PROTOBUF_C_MESSAGE_DESCRIPTOR_MAGIC,
  "DataPoints",
  "DataPoints",
  "DataPoints",
  "",
  sizeof(DataPoints),
  2,
  data_points__field_descriptors,
  data_points__field_indices_by_name,
  1,  data_points__number_ranges,
  (ProtobufCMessageInit) data_points__init,
  NULL,NULL,NULL    /* reserved[123] */
};
